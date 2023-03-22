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
    , {create_calldata_call, undefined, "call", string,
        "Calldata creation - the call, e.g. \"foo(42, true)\""}
    , {decode_call_res, undefined, "call_result_type", {string, "ok"},
        "Decode contract call result - 'ok' | 'revert' | 'error'"}
    , {decode_call_val, undefined, "call_result", string,
        "Decode contract call result - input is contract bytearray (cb_...) or string"}
    , {decode_call_fun, undefined, "call_result_fun", string,
        "Decode contract call result - function name"}
    , {decode_calldata, undefined, "decode_calldata", string,
        "Decode calldata - input is contract bytearray (cb_...)"}
    , {decode_calldata_fun, undefined, "calldata_fun", string,
        "Decode calldata - function name"}
    , {include_path, $i, "include_path", string, "Explicit include path"}
    , {no_warning, $w, "no_warning", string,
        "Disabled warnings; " ++ string:join([W || {W, _} <- all_warnings()], " | ")}
    , {error_warning, $e, "error_warning", undefined, "Report warnings as errors"}
    , {to_validate, undefined, "validate", string,
        "Verify that a contract bytearray (cb_...) is the result of compiling the given source code"}
    , {compiled_by, undefined, "compiled_by", string, "Extract compiler version from file (.aeb) or bytearray (cb_...)"}
    , {outfile, $o, "out", string, "Output the result to file"}
    , {verbose, $v, "verbose", undefined, "Verbose output"}
    , {pp_asm,  undefined, "pp_asm", undefined, "Pretty print assembler code after compilation"}
    , {pp_size, undefined, "pp_size", undefined, "Print the size of the compiled byte code"}
    , {oneline_errors, undefined, "oneline_errors", undefined, "Print errors on one (long) line"}
    , {version, undefined, "version", undefined, "Sophia compiler version"}]).

usage() ->
    getopt:usage(?OPT_SPEC, "aesophia_cli"),
    timer:sleep(10),
    io:format("EXAMPLES:\n"
              "[compile] :\n"
              "  aesophia_cli identity.aes -o identity.aeb\n"
              "[compile (with unused functions and shadowing warnings disabled)] :\n"
              "  aesophia_cli -wunused_functions -wshadowing identity.aes\n"
              "[compile with explicit include path] :\n"
              "  aesophia_cli identity.aes -i /path/to/include/ -o identity.aeb\n"
              "[extract aesophia compiler version] :\n"
              "  aesophia_cli --compiled_by cb_+GZGA6CpNW171TSU...MAANEx2r\n"
              "[create aci stub] : \n"
              "  aesophia_cli --create_stub_aci identity.aes\n"
              "[create aci JSON] : \n"
              "  aesophia_cli --create_json_aci identity.aes -o identity.json\n"
              "[create calldata] :\n"
              "  aesophia_cli --create_calldata identity.aes --call \"main_(42)\"\n"
              "[decode calldata] :\n"
              "  aesophia_cli --decode_calldata cb_KxG3+3bAG1StlAV3 --calldata_fun main_ identity.aes\n"
             ),
    error.

main(Args) ->
    case main1(Args) of
        ok -> ok;
        _  -> erlang:halt(1)
    end.

main1(Args) ->
    case getopt:parse(?OPT_SPEC, Args) of
        {ok, {Opts, []}} ->
            IsHelp         = proplists:get_value(help, Opts, false),
            IsVersion      = proplists:get_value(version, Opts, false),
            CreateCallData = proplists:get_value(create_calldata, Opts, undefined),
            DecodeCall     = proplists:get_value(decode_call_val, Opts, undefined),
            DecodeCallData = proplists:get_value(decode_calldata, Opts, undefined),
            CreateACIJSON  = proplists:get_value(create_json_aci, Opts, undefined),
            CreateACIStub  = proplists:get_value(create_stub_aci, Opts, undefined),
            CompiledBy     = proplists:get_value(compiled_by, Opts, undefined),
            Validate       = proplists:get_value(to_validate, Opts, undefined),
            if  IsHelp ->
                    usage();
                CreateCallData /= undefined ->
                    create_calldata(CreateCallData, Opts);
                DecodeCall /= undefined ->
                    decode_call_res(DecodeCall, Opts);
                DecodeCallData /= undefined ->
                    decode_calldata(DecodeCallData, Opts);
                CreateACIJSON /= undefined ->
                    create_aci(json, CreateACIJSON, Opts);
                CreateACIStub /= undefined ->
                    create_aci(stub, CreateACIStub, Opts);
                CompiledBy /= undefined ->
                    compiled_by(CompiledBy, Opts);
                Validate /= undefined ->
                    validate(Validate, Opts);
                IsVersion ->
                    {ok, Vsn} = aeso_compiler:version(),
                    io:format("Sophia compiler version ~s\n", [Vsn]);
                true ->
                    compile(Opts)
            end;

        {ok, {_, NonOpts}} ->
            io:format(standard_error, "Can't understand ~p\n\n", [NonOpts]),
            usage();

        {error, {Reason, Data}} ->
            io:format(standard_error, "Error: ~s ~p\n\n", [Reason, Data]),
            usage()
    end.

with_input_file(Opts, Fun) ->
    case proplists:get_value(src_file, Opts, undefined) of
        undefined ->
            io:format(standard_error, "Error: no input source file\n\n", []),
            usage();
        File ->
            Fun(File)
    end.

compile(Opts) ->
    with_input_file(Opts, fun(File) -> compile(File, Opts) end).

compile(File, Opts) ->
    OutFile = proplists:get_value(outfile, Opts, undefined),
    case aeso_compiler:file(File, get_compiler_opts(Opts)) of
        {ok, Map} ->
            write_bytecode(OutFile, Map, Opts);
        {error, Reasons} ->
            pp_errors(Reasons, Opts),
            {error, Reasons}
    end.

validate(ByteCode, Opts) ->
    with_input_file(Opts, fun(File) ->
        case file:read_file(File) of
            {error, Error} ->
                Msg = lists:flatten([File,": ",file:format_error(Error)]),
                io:format(standard_error, "~s\n", [aeso_errors:pp(aeso_errors:new(file_error, Msg))]);
            {ok, Src} ->
                COpts = get_compiler_opts(Opts),
                case aeser_api_encoder:decode(list_to_binary(ByteCode)) of
                    {contract_bytearray, Bin} ->
                        Map = aeser_contract_code:deserialize(Bin),
                        case aeso_compiler:validate_byte_code(Map, binary_to_list(Src), COpts) of
                            ok ->
                                io:format("Validation successful.\n");
                            {error, Reasons} ->
                                pp_errors(Reasons, Opts),
                                {error, Reasons}
                        end;
                    Err ->
                        io:format(standard_error, "~p\n", [Err])
                end
        end
    end).

compiled_by(Input0, _Opts) ->
    %% Don't try to be clever first see if it is a file...
    Input = case file:read_file(Input0) of
                {ok, BinInput} -> BinInput;
                {error, _}     -> list_to_binary(Input0)
            end,
    case aeser_api_encoder:safe_decode(contract_bytearray, Input) of
        {ok, Bin} -> compiled_by(Bin);
        {error, _} ->
            io:format(standard_error, "ERROR: Input is neither a file or a contract bytearray (cb_...)\n", [])
    end.

compiled_by(Bin) ->
    try
        Map = aeser_contract_code:deserialize(Bin),
        CVer = maps:get(compiler_version, Map, undefined),
        io:format("~s\n", [CVer])
    catch _:_ ->
        io:format(standard_error, "ERROR: Could not deserialize contract binary\n", [])
    end.

create_aci(Type, ContractFile, Opts) ->
    OutFile = proplists:get_value(outfile, Opts, undefined),
    IncPath = get_inc_path(Opts),
    Verbose = get_verbose(Opts),
    case aeso_aci:file(json, ContractFile, Verbose ++ IncPath) of
        {ok, Enc} ->
            case Type of
                json ->
                    write_aci(OutFile, io_lib:format("~s\n", [jsx:encode(Enc)]));
                stub ->
                    {ok, Stub} = aeso_aci:render_aci_json(Enc),
                    write_aci(OutFile, io_lib:format("~s\n", [Stub]))
            end;
        {error, Reasons} ->
            pp_errors(Reasons, Opts),
            {error, Reasons}
    end.

create_calldata(ContractFile, Opts) ->
    case file:read_file(ContractFile) of
        {ok, Bin} ->
            Code = binary_to_list(Bin),
            create_calldata_(Code, Opts, get_inc_path(ContractFile, Opts));
        {error, _} ->
            io:format(standard_error, "Error: Could not find file ~s\n\n", [ContractFile]),
            usage()
    end.

create_calldata_(Contract, Opts, COpts) ->
    case proplists:get_value(create_calldata_call, Opts, undefined) of
        undefined ->
            io:format(standard_error, "Error: missing 'call' parameter for calldata creation\n\n", []), usage();
        Call ->
            case prepare_call(Call) of
                {ok, Fun, Args} ->
                    create_calldata(Contract, Fun, Args, Opts, COpts);
                {error, Reason} when is_atom(Reason) ->
                    {error, Reason};
                {error, Reasons} ->
                    io:format(standard_error,
                              "Error: could not parse the arguments, "
                              "they should be one string with comma separated literals.\n",
                              []),
                    pp_errors(Reasons, Opts),
                    {error, Reasons}
            end
    end.

create_calldata(Contract, CallFun, CallArgs, Opts, COpts) ->
    OutFile = proplists:get_value(outfile, Opts, undefined),

    case aeso_compiler:create_calldata(Contract, CallFun, CallArgs, COpts) of
        {ok, CallData} ->
            write_calldata(OutFile, CallData);
        {error, Reasons} ->
            pp_errors(Reasons, Opts),
            {error, Reasons}
    end.

decode_call_res(EncValue, Opts) ->
    case proplists:get_value(src_file, Opts, undefined) of
        undefined ->
            io:format(standard_error, "Error: no input source file\n\n", []),
            usage();
        File ->
            case file:read_file(File) of
                {ok, Bin} ->
                    Code = binary_to_list(Bin),
                    decode_call_res(EncValue, Code, Opts, get_inc_path(File, Opts));
                {error, _} ->
                    io:format(standard_error, "Error: Could not find file ~s\n", [File])
            end
    end.

decode_call_res(EncValue, Source, Opts, COpts) ->
    case proplists:get_value(decode_call_fun, Opts, undefined) of
        undefined ->
            io:format(standard_error, "Error: no --call_result_fun given\n", []),
            usage();
        FunName ->
            case aeser_api_encoder:safe_decode(contract_bytearray, list_to_binary(EncValue)) of
                {ok, CallValue} ->
                    decode_call_res(Source, FunName, proplists:get_value(decode_call_res, Opts), CallValue, Opts, COpts);
                {error, _} = Err ->
                    io:format(standard_error, "Error: Bad call result value\n", []),
                    Err
            end
    end.

decode_call_res(Source, FunName, CallRes0, CallValue, Opts, COpts) ->
    CallRes = erlang:list_to_atom(CallRes0),
    case aeso_compiler:to_sophia_value(Source, FunName, CallRes, CallValue, COpts) of
        {ok, Ast} ->
            io:format("Decoded call result:\n~s\n", [prettypr:format(aeso_pretty:expr(Ast))]);
        {error, Reasons} ->
            pp_errors(Reasons, Opts),
            {error, Reasons}
    end.

decode_calldata(EncValue, Opts) ->
    case proplists:get_value(src_file, Opts, undefined) of
        undefined ->
            io:format(standard_error, "Error: no input source file\n\n", []),
            usage();
        File ->
            case file:read_file(File) of
                {ok, Bin} ->
                    Code = binary_to_list(Bin),
                    decode_calldata(EncValue, Code, Opts, get_inc_path(File, Opts));
                {error, _} ->
                    io:format(standard_error, "Error: Could not find file ~s\n", [File])
            end
    end.

decode_calldata(EncValue, Source, Opts, COpts) ->
    case proplists:get_value(decode_calldata_fun, Opts, undefined) of
        undefined ->
            io:format(standard_error, "Error: no --calldata_fun given\n", []),
            usage();
        FunName ->
            case aeser_api_encoder:safe_decode(contract_bytearray, list_to_binary(EncValue)) of
                {ok, Calldata} ->
                    decode_calldata(Source, FunName, Calldata, Opts, COpts);
                {error, _} = Err ->
                    io:format(standard_error, "Error: Bad calldata value\n", []),
                    Err
            end
    end.

decode_calldata(Source, FunName, Calldata, Opts, COpts) ->
    case aeso_compiler:decode_calldata(Source, FunName, Calldata, COpts) of
        {ok, _Types, ArgAsts} ->
            Args = lists:join(", ", [ prettypr:format(aeso_pretty:expr(ArgAst)) || ArgAst <- ArgAsts ]),
            io:format("Decoded calldata:\n~s(~s)\n", [FunName, Args]);
        {error, Reasons} ->
            pp_errors(Reasons, Opts),
            {error, Reasons}
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

write_bytecode(OutFile, CompileMap = #{ contract_source := SourceStr, warnings := Warnings }, Opts) ->
    %% Print warnings first
    [io:format(standard_error, "~s\n", [aeso_warnings:pp(Warn)]) || Warn <- Warnings],
    %% eblake2 is slow - but NIFs don't work in escript (easily...)
    {ok, SourceHash} = eblake2:blake2b(32, list_to_binary(SourceStr)),
    SerByteCode = aeser_contract_code:serialize(CompileMap#{ source_hash => SourceHash }),
    case proplists:get_value(pp_size, Opts, false) of
        true ->
            io:format("~p\n", [byte_size(SerByteCode)]);
        false ->
            ByteCode = aeser_api_encoder:encode(contract_bytearray, SerByteCode),
            case OutFile of
                undefined ->
                    case proplists:get_value(pp_asm, Opts, false) of
                        true -> ok;
                        false -> io:format("~s\n", [ByteCode])
                    end;
                _ ->
                    io:format("Compiled successfully!\n"),
                    io:format("Output written to: ~s\n\n", [OutFile]),
                    file:write_file(OutFile, ByteCode)
            end
    end.

write_aci(undefined, ACI) ->
    io:format("~s", [ACI]);
write_aci(OutFile, ACI) ->
    io:format("Output written to: ~s\n", [OutFile]),
    file:write_file(OutFile, ACI).

%% Maybe better to do on the compiler side...
prepare_call(Call) ->
    try aeso_parser:string("main contract C =\n function foo() = " ++ aeso_scan:utf8_encode(Call)) of
        [{contract_main, _, _, _, [{letfun, _, _, _, _, [{guarded, _, [], {app, _, Fun0, Args0}}]}]}] ->
            {id, _, Fun} = Fun0,
            Args = [prepare_arg(Arg) || Arg <- Args0],
            {ok, Fun, Args};
        _ ->
            io:format(standard_error, "Bad 'call' - it should be \"fun(arg1, arg2, ...)\"\n", []),
            {error, input_error}
    catch throw:{error, Reasons} ->
        {error, Reasons}
    end.

prepare_arg({string, _, <<>>}) -> "\"\"";
prepare_arg(Arg)               -> no_nl(prettypr:format(aeso_pretty:expr(Arg))).

no_nl(Str) -> lists:flatten(string:replace(Str, "\n", "", all)).

get_compiler_opts(Opts) ->
    IncludePath = get_inc_path(Opts),
    Verbose     = get_verbose(Opts),
    PPAsm       = get_pp_asm(Opts),
    Warnings    = get_warnings(Opts),
    Verbose ++ IncludePath ++ PPAsm ++ Warnings.

get_inc_path(File, Opts) ->
    aeso_compiler:add_include_path(File, get_inc_path(Opts)).

get_inc_path(Opts) ->
    case [ Path || {include_path, Path} <- Opts ] of
        []    -> [];
        Paths -> [{include, {file_system, Paths}}]
    end.

get_verbose(Opts) ->
    case proplists:get_value(verbose, Opts, false) of
        false -> [];
        true  -> [pp_ast]
    end.

get_pp_asm(Opts) ->
    case proplists:get_value(pp_asm, Opts, false) of
        false -> [];
        true  -> [pp_assembler]
    end.

all_warnings() ->
    % New warnings should be added here after they're added to aesophia
    [ {"all", warn_all}
    , {"unused_includes", warn_unused_includes}
    , {"unused_stateful", warn_unused_stateful}
    , {"unused_variables", warn_unused_variables}
    , {"unused_typedefs", warn_unused_typedefs}
    , {"unused_return_value", warn_unused_return_value}
    , {"unused_functions", warn_unused_functions}
    , {"shadowing", warn_shadowing}
    , {"division_by_zero", warn_division_by_zero}
    , {"negative_spend", warn_negative_spend} ].

get_warnings(Opts) ->
    DisabledWarnings = [W || {no_warning, W} <- Opts],
    Warnings = case DisabledWarnings of
                   [] -> [warn_all];
                   _  -> case lists:member("all", DisabledWarnings) of
                             true  -> [];
                             false -> [proplists:get_value(W, all_warnings()) ||
                                       W <- DisabledWarnings,
                                       W /= undefined]
                         end
               end,
    case lists:member(error_warning, Opts) of
        true  -> Warnings ++ [warn_error];
        false -> Warnings
    end.

pp_errors(Reasons, Opts) ->
    case proplists:get_value(oneline_errors, Opts, false) of
        true  ->
            Errors = lists:join("\n", [ aeso_errors:pp_oneline(R) || R <- Reasons ]),
            io:format(standard_error, "~s\n", [Errors]);
        false ->
            Errors = lists:join("\n", [ aeso_errors:pp(R) || R <- Reasons ]),
            io:format(standard_error, "~s", [Errors])
    end.
