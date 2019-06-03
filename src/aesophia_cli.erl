-module(aesophia_cli).

-export([main/1]).

-define(OPT_SPEC,
    [ {src_file, undefined, undefined, string, "Sophia source code file"}
    , {help, $h, "help", undefined, "Show this message"}
    , {create_json_aci, undefined, "create_json_aci", string,
        "Create ACI in JSON format"}
    , {create_stub_aci, undefined, "create_stub_aci", string,
        "Create ACI stub-contract"}
    , {create_calldata, undefined, "create_calldata", string,
        "Create calldata with respect to (stub-)contract in this file"}
    , {create_calldata_fun, undefined, "calldata_fun", string,
        "Calldata creation - name of function"}
    , {create_calldata_args, undefined, "calldata_args", string,
        "Calldata creation - function arguments, e.g. \"42, true, [1, 3, 7]\""}
    , {decode_data, undefined, "decode_data", string,
        "Decode contract call result, input is a contract bytearray (cb_...)"}
    , {decode_data_type, undefined, "decode_type", string,
        "The Sophia type to decode data into"}
    , {decode_call_res, undefined, "call_result_type", {string, "ok"},
        "Decode contract call result - 'ok' | 'revert' | 'error'"}
    , {decode_call_val, undefined, "call_result", string,
        "Decode contract call result - input is contract bytearray (cb_...) or string"}
    , {decode_call_fun, undefined, "call_result_fun", string,
        "Decode contract call result - function name"}
    , {outfile, $o, "out", string, "Output the result to file"}
    , {verbose, $v, "verbose", undefined, "Verbose output"}
    , {version, undefined, "version", undefined, "Sophia compiler version"}]).

usage() ->
    getopt:usage(?OPT_SPEC, "aesophia_cli"),
    timer:sleep(10),
    io:format("EXAMPLES:\n"
              "[compile] :\n"
              "  aesophia_cli identity.aes -o identity.aeb\n"
              "[create aci stub] : \n"
              "  aesophia_cli --create_stub_aci identity.aes\n"
              "[create aci JSON] : \n"
              "  aesophia_cli --create_json_aci identity.aes -o identity.json\n"
              "[create calldata] :\n"
              "  aesophia_cli --create_calldata identity.aes --calldata_fun main --calldata_args 42\n"
              "[decode call result] : \n"
              "  aesophia_cli identity.aes --call_result cb_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACr8s/aY --call_result_fun main\n"
              "[decode data] :\n"
              "  aesophia_cli --decode_data cb_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACr8s/aY --decode_type int\n\n").

main(Args) ->
    case getopt:parse(?OPT_SPEC, Args) of
        {ok, {Opts, []}} ->
            IsHelp         = proplists:get_value(help, Opts, false),
            IsVersion      = proplists:get_value(version, Opts, false),
            CreateCallData = proplists:get_value(create_calldata, Opts, undefined),
            DecodeData     = proplists:get_value(decode_data, Opts, undefined),
            DecodeCall     = proplists:get_value(decode_call_val, Opts, undefined),
            CreateACIJSON  = proplists:get_value(create_json_aci, Opts, undefined),
            CreateACIStub  = proplists:get_value(create_stub_aci, Opts, undefined),
            if  IsHelp ->
                    usage();
                CreateCallData /= undefined ->
                    create_calldata(CreateCallData, Opts);
                DecodeCall /= undefined ->
                    decode_call_res(DecodeCall, Opts);
                DecodeData /= undefined ->
                    decode_data(DecodeData, Opts);
                CreateACIJSON /= undefined ->
                    create_aci(json, CreateACIJSON, Opts);
                CreateACIStub /= undefined ->
                    create_aci(stub, CreateACIStub, Opts);
                IsVersion ->
                    {ok, Vsn} = aeso_compiler:version(),
                    io:format("Sophia compiler version ~s\n", [Vsn]);
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
            io:format("Error:\n~s\n\n", [Reason]),
            {error, Reason}
    catch
        error:Error ->
            Where = hd(erlang:get_stacktrace()),
            ErrorString = io_lib:format("Error: ~p in\n   ~p", [Error, Where]),
            io:format("~s\n", [ErrorString]),
            {error, list_to_binary(lists:flatten(ErrorString))}
    end.

create_aci(Type, ContractFile, Opts) ->
    case file:read_file(ContractFile) of
        {ok, Bin} ->
            Code = binary_to_list(Bin),
            create_aci_(Type, Code, Opts);
        {error, _} ->
            io:format("Error: Could not find file ~s\n\n", [ContractFile]),
            usage()
    end.

create_aci_(Type, Code, Opts) ->
    Verbose = proplists:get_value(verbose, Opts, false),
    OutFile = proplists:get_value(outfile, Opts, undefined),
    try aeso_aci:contract_interface(json, Code, [pp_ast || Verbose]) of
        {ok, Enc} ->
            io:format("ACI generated successfully!\n\n"),
            case Type of
                json ->
                    write_aci(OutFile, io_lib:format("~s\n", [jsx:encode(Enc)]));
                stub ->
                    {ok, Stub} = aeso_aci:render_aci_json(Enc),
                    write_aci(OutFile, io_lib:format("~s\n", [Stub]))
            end;
        {error, Reason} ->
            io:format("Error: Failed to create ACI - ~s\n", [Reason]),
            {error, Reason}
    catch _:Reason ->
        io:format("Error: Failed to create ACI - ~p\n", [Reason]),
        {error, Reason}
    end.

create_calldata(ContractFile, Opts) ->
    case file:read_file(ContractFile) of
        {ok, Bin} ->
            Code = binary_to_list(Bin),
            create_calldata_(Code, Opts);
        {error, _} ->
            io:format("Error: Could not find file ~s\n\n", [ContractFile]),
            usage()
    end.

create_calldata_(Contract, Opts) ->
    case {proplists:get_value(create_calldata_fun, Opts, undefined),
          proplists:get_value(create_calldata_args, Opts, undefined)} of
        {undefined, _} ->
            io:format("Error: not enough create call data input\n\n"), usage();
        {_, undefined} ->
            io:format("Error: not enough create call data input\n\n"), usage();
        {Fun, Args0} ->
            case prepare_args(Args0) of
                {ok, Args} ->
                    create_calldata(Contract, Fun, Args, Opts);
                error ->
                    io:format("Error: could not parse the arguments, "
                              "they should be one string with comma separated literals.\n")
            end
    end.

create_calldata(Contract, CallFun, CallArgs, Opts) ->
    OutFile = proplists:get_value(outfile, Opts, undefined),
    try
        case aeso_compiler:create_calldata(Contract, CallFun, CallArgs) of
            {ok, CallData, _CallDataType, _OutputType} ->
                write_calldata(OutFile, CallData);
            Err = {error, Reason} ->
                io:format("Error: Create calldata failed:\n~s\n", [Reason]),
                Err
        end
    catch error:Error ->
        Where = hd(erlang:get_stacktrace()),
        ErrorString = io_lib:format("Error: ~p in\n   ~p", [Error, Where]),
        io:format("~s\n", [ErrorString]),
        {error, list_to_binary(lists:flatten(ErrorString))}
    end.

decode_call_res(EncValue, Opts) ->
    case proplists:get_value(src_file, Opts, undefined) of
        undefined ->
            io:format("Error: no input source file\n\n"),
            usage();
        File ->
            case file:read_file(File) of
                {ok, Bin} ->
                    Code = binary_to_list(Bin),
                    decode_call_res(EncValue, Code, Opts);
                {error, _} ->
                    io:format("Error: Could not find file ~s\n", [File])
            end
    end.

decode_call_res(EncValue, Source, Opts) ->
    case proplists:get_value(decode_call_fun, Opts, undefined) of
        undefined ->
            io:format("Error: no --call_result_fun given\n"),
            usage();
        FunName ->
            case aeser_api_encoder:safe_decode(contract_bytearray, list_to_binary(EncValue)) of
                {ok, CallValue} ->
                    decode_call_res(Source, FunName, proplists:get_value(decode_call_res, Opts), CallValue);
                {error, _} = Err ->
                    io:format("Error: Bad call result value\n"),
                    Err
            end
    end.

decode_call_res(Source, FunName, CallRes, CallValue) ->
    case aeso_compiler:to_sophia_value(Source, FunName,erlang:list_to_atom(CallRes), CallValue) of
        {ok, Ast} ->
            io:format("Decoded call result:\n~s\n", [prettypr:format(aeso_pretty:expr(Ast))]);
        Err = {error, Reason} ->
            io:format("Error: Failed to decode call result - ~p\n", [Reason]),
            Err
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
            try aeb_heap:from_binary(TypeRep, Data) of
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

write_aci(undefined, ACI) ->
    io:format("~s", [ACI]);
write_aci(OutFile, ACI) ->
    io:format("Output written to: ~s\n", [OutFile]),
    file:write_file(OutFile, ACI).

%% Maybe better to do on the compiler side...
prepare_args("") ->
    {ok, []};
prepare_args(ArgsStr) ->
    case aeso_parser:string("contract C =\n  function foo() = (" ++ ArgsStr ++ ")") of
        {ok, [{contract, _, _, [{letfun, _, _, _, _, Args}]}]} ->
            case Args of
                {tuple, _, Args1} -> {ok, [prepare_arg(Arg) || Arg <- Args1]};
                _                 -> {ok, [prepare_arg(Args)]}
            end;
        _ ->
            error
    end.

prepare_arg({string, _, <<>>}) -> "\"\"";
prepare_arg(Arg)               -> prettypr:format(aeso_pretty:expr(Arg)).
