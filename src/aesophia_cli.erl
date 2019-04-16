-module(aesophia_cli).

-export([main/1]).

-define(OPT_SPEC,
    [ {src_file, undefined, undefined, string, "Sophia source code file"}
    , {help, $h, "help", undefined, "Show this message"}
    , {create_calldata, $c, "create_calldata", string,
        "Create calldata with respect to compiled contract in this file"}
    , {create_calldata_fun, undefined, "calldata_fun", string,
        "Calldata creation - using function + arguments - function"}
    , {create_calldata_args, undefined, "calldata_args", string,
        "Calldata creation - using function + arguments - arguments"}
    , {decode_data, undefined, "decode_data", string,
        "Decode contract call result, input is a contract bytearray (cb_...)"}
    , {decode_data_type, undefined, "decode_type", string,
        "The Sophia type to decode data into"}
    , {outfile, $o, "out", string, "Output the result to file"}
    , {verbose, $v, "verbose", undefined, "Verbose output"}
    , {version, undefined, "version", undefined, "Sophia compiler version"}]).

usage() ->
    getopt:usage(?OPT_SPEC, "aesophia_cli"),
    timer:sleep(10),
    io:format("EXAMPLES:\n"
              "[compile] :\n"
              "  aesophia_cli identity.aes -o identity.aeb\n"
              "[create calldata] :\n"
              "  aesophia_cli --create_calldata identity.aeb --calldata_fun main --calldata_args 42\n"
              "[decode data] :\n"
              "  aesophia_cli --decode_data cb_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACr8s/aY --decode_type int\n\n").

main(Args) ->
    case getopt:parse(?OPT_SPEC, Args) of
        {ok, {Opts, []}} ->
            IsHelp = proplists:get_value(help, Opts, false),
            IsVersion = proplists:get_value(version, Opts, false),
            CreateCallData = proplists:get_value(create_calldata, Opts, undefined),
            DecodeData = proplists:get_value(decode_data, Opts, undefined),
            if  IsHelp ->
                    usage();
                CreateCallData /= undefined ->
                    create_calldata(CreateCallData, Opts);
                DecodeData /= undefined ->
                    decode_data(DecodeData, Opts);
                IsVersion ->
                    io:format("Sophia compiler version 1.4.0\n", []);
                true ->
                    compile(Opts)
            end;

        {ok, {_, NonOpts}} ->
            io:format("Can't understand ~p\n\n", [NonOpts]),
            usage();

        {error, {Reason, Data}} ->
            io:format("Error: ~s ~p\n\n", [Reason, Data]),
            usage()
    end.


compile(Opts) ->
    case proplists:get_value(src_file, Opts, undefined) of
        undefined ->
            io:format("Error: no input source file\n\n"),
            usage();
        File ->
            compile(File, Opts)
    end.

compile(File, Opts) ->
    Verbose = proplists:get_value(verbose, Opts, false),
    OutFile = proplists:get_value(outfile, Opts, undefined),

    try aeso_compiler:file(File, [pp_ast || Verbose]) of
        {ok, Map} ->
            write_bytecode(OutFile, Map);
        {error, Reason} ->
            io:format("Error: ~p\n\n", [Reason]),
            {error, Reason}
    catch
        error:Error ->
            Where = hd(erlang:get_stacktrace()),
            ErrorString = io_lib:format("Error: ~p in\n   ~p", [Error, Where]),
            io:format("~s\n", [ErrorString]),
            {error, list_to_binary(lists:flatten(ErrorString))}
    end.


create_calldata(ContractFile, Opts) ->
    case file:read_file(ContractFile) of
        {ok, Bin} ->
            case aeser_api_encoder:safe_decode(contract_bytearray, Bin) of
                {ok, SerContract} ->
                    try
                        {ok, Contract} = aescli_ser:deserialize(SerContract),
                        create_calldata_(Contract, Opts)
                    catch _:_ ->
                        io:format("Error: Bad contract file ~s\n\n", [ContractFile]), usage()
                    end;
                {error, _} ->
                    io:format("Bad input format in file ~s, expected "
                              "'contract_bytearray'\n", [ContractFile])
            end;
        {error, _} ->
            io:format("Error: Could not find file ~s\n\n", [ContractFile]), usage()
    end.


create_calldata_(Contract, Opts) ->
    case proplists:get_value(src_file, Opts, undefined) of
        undefined -> %% Check if old deprecated style is used
            case {proplists:get_value(create_calldata_fun, Opts, undefined),
                  proplists:get_value(create_calldata_args, Opts, undefined)} of
                {undefined, _} ->
                    io:format("Error: not enough create call data input\n\n"), usage();
                {_, undefined} ->
                    io:format("Error: not enough create call data input\n\n"), usage();
                {Fun, Args} ->
                    create_calldata(Contract, Fun, Args, Opts)
            end;
        CallFile ->
            case file:read_file(CallFile) of
                {ok, Bin} ->
                    create_calldata(Contract, "", binary_to_list(Bin), Opts);
                {error, _} ->
                    io:format("Error: Could not find file ~s\n\n", [CallFile]), usage()
            end
    end.

create_calldata(Contract, CallFun, CallArgs, Opts) ->
    OutFile = proplists:get_value(outfile, Opts, undefined),

    try
        case aeso_compiler:create_calldata(Contract, CallFun, CallArgs) of
            {ok, CallData, _CallDataType, _OutputType} ->
                write_calldata(OutFile, CallData);
            Err = {error, Reason} ->
                io:format("Error: Create calldata failed: ~p\n\n", [Reason]),
                Err
        end
    catch error:Error ->
        Where = hd(erlang:get_stacktrace()),
        ErrorString = io_lib:format("Error: ~p in\n   ~p", [Error, Where]),
        io:format("~s\n", [ErrorString]),
        {error, list_to_binary(lists:flatten(ErrorString))}
    end.

decode_data(EncData, Opts) ->
    case aeser_api_encoder:safe_decode(contract_bytearray, list_to_binary(EncData)) of
        {ok, Data} ->
            case proplists:get_value(decode_data_type, Opts, undefined) of
                undefined ->
                    io:format("Error: Missing 'decode_type` parameter\n");
                SophiaType ->
                    decode_data_(Data, SophiaType, Opts)
            end;
        Err = {error, Reason} ->
            io:format("Error: Bad data - ~p\n", [Reason]),
            Err
    end.

decode_data_(Data, SophiaType, _Opts) ->
    case aeso_compiler:sophia_type_to_typerep(SophiaType) of
        {ok, TypeRep} ->
            try aeso_heap:from_binary(TypeRep, Data) of
                {ok, Term} ->
                    io:format("Decoded data:\n~p\n", [Term]);
                Err = {error, Reason} ->
                    io:format("Error: Failed to decode data - ~p\n", [Reason]),
                    Err
            catch _T:Reason ->
                io:format("Error: Failed to decode data - ~p\n", [Reason]),
                {error, bad_type_or_data}
            end;
        Err = {error, Reason} ->
            io:format("Error: Bad type - ~p\n", [Reason]),
            Err
    end.

write_calldata(OutFile, CallData) ->
    EncCallData = aeser_api_encoder:encode(contract_bytearray, CallData),
    case OutFile of
        undefined ->
            io:format("Calldata:\n~s\n", [EncCallData]);
        _ ->
            io:format("Calldata created successfully!\n"),
            io:format("Output written to: ~s\n\n", [OutFile]),
            file:write_file(OutFile, EncCallData)
    end.

write_bytecode(OutFile, CompileMap) ->
    SerByteCode = aescli_ser:serialize(CompileMap),
    ByteCode = aeser_api_encoder:encode(contract_bytearray, SerByteCode),
    case OutFile of
        undefined ->
            io:format("Bytecode:\n~s\n", [ByteCode]);
        _ ->
            io:format("Compiled successfully!\n"),
            io:format("Output written to: ~s\n\n", [OutFile]),
            file:write_file(OutFile, ByteCode)
    end.
