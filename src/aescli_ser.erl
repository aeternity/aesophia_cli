%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc Sophia contract serialization
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(aescli_ser).

-export([ serialize/1
        , deserialize/1
        ]).

%% -- Contract serialization
-define(SOPHIA_CONTRACT_VSN, 2).
-define(SOPHIA_CONTRACT_VSN_1, 1).
-define(SOPHIA_CONTRACT_VSN_2, 2).
-define(COMPILER_SOPHIA_TAG, compiler_sophia).

serialize(CompileMap) ->
    serialize(?SOPHIA_CONTRACT_VSN, CompileMap).

serialize(Vsn, #{byte_code := ByteCode, type_info := TypeInfo,
                 contract_source := ContractString, compiler_version := Version}) ->
    ContractBin      = list_to_binary(ContractString),
    {ok, SourceHash} = eblake2:blake2b(32, ContractBin),
    Fields = [ {source_hash, SourceHash}
             , {type_info, TypeInfo}
             , {byte_code, ByteCode}]
             ++ [ {compiler_version, Version} || Vsn == ?SOPHIA_CONTRACT_VSN_2 ],
    aeser_chain_objects:serialize(?COMPILER_SOPHIA_TAG, Vsn,
                                  serialization_template(Vsn), Fields).

deserialize(Binary) ->
    case aeser_chain_objects:deserialize_type_and_vsn(Binary) of
        {compiler_sophia = Type, ?SOPHIA_CONTRACT_VSN_1 = Vsn, _Rest} ->
            Template = serialization_template(Vsn),
            [ {source_hash, Hash}
            , {type_info, TypeInfo}
            , {byte_code, ByteCode}
            ] = aeser_chain_objects:deserialize(Type, Vsn, Template, Binary),
            {ok, #{ source_hash => Hash
                  , type_info => TypeInfo
                  , byte_code => ByteCode
                  , contract_vsn => Vsn
                  }};
        {compiler_sophia = Type, ?SOPHIA_CONTRACT_VSN_2 = Vsn, _Rest} ->
            Template = serialization_template(Vsn),
            [ {source_hash, Hash}
            , {type_info, TypeInfo}
            , {byte_code, ByteCode}
            , {compiler_version, CompilerVersion}
            ] = aeser_chain_objects:deserialize(Type, Vsn, Template, Binary),
            {ok, #{ source_hash => Hash
                  , type_info => TypeInfo
                  , byte_code => ByteCode
                  , compiler_version => CompilerVersion
                  , contract_vsn => Vsn
                  }};
        Other ->
            {error, {illegal_code_object, Other}}
    end.

serialization_template(?SOPHIA_CONTRACT_VSN_1) ->
    [ {source_hash, binary}
    , {type_info, [{binary, binary, binary, binary}]} %% {type hash, name, arg type, out type}
    , {byte_code, binary} ];
serialization_template(?SOPHIA_CONTRACT_VSN_2) ->
    [ {source_hash, binary}
    , {type_info, [{binary, binary, binary, binary}]} %% {type hash, name, arg type, out type}
    , {byte_code, binary}
    , {compiler_version, binary} ].

