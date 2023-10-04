aesophia_cli
=====

A command line interface for the [Sophia compiler](https://github.com/aeternity/aesophia).

The result of compiling a contract is `Sophia byte code` as documented in
[Aeternity protocol](https://github.com/aeternity/protocol/blob/master/serializations.md#sophia-byte-code-version-3-lima-release).

Build
-----

You need Erlang/OTP (version 23 or newer) installed to build the compiler CLI.

    $ ./rebar3 escriptize

Run
---

    $ ./aesophia_cli


Examples
--------

Simple test contracts in `test/contracts/`.


```
[compile] :
  aesophia_cli identity.aes -o identity.aeb
[compile (with unused functions and shadowing warnings enabled)] :
  aesophia_cli -wunused_functions -wshadowing identity.aes
[compile with explicit include path] :
  aesophia_cli identity.aes -i /path/to/include/ -o identity.aeb
[create aci stub] :
  aesophia_cli --create_stub_aci identity.aes
[create aci JSON] :
  aesophia_cli --create_json_aci identity.aes -o identity.json
[create calldata] :
  aesophia_cli --create_calldata identity.aes --call "main_(42)"
[decode calldata] :
  aesophia_cli --decode_calldata cb_KxG3+3bAG1StlAV3 --calldata_fun main_ identity.aes
[decode call result] :
  aesophia_cli --call_result cb_VNLOFXc= --call_result_type ok --call_result_fun main_ identity.aes
[validate byte code] :
  aesophia_cli --validate cb_+GdGA6C7MajPXETkDK+UVqKYiFfJYQR/4jFNiD8Vqjl3gxZWvsC4Op7+RNZEHwA3ADcAGg6CPwEDP/63+3bAADcBBwcBAQCXLwIRRNZEHxFpbml0Ebf7dsAVbWFpbl+CLwCFNy4xLjAA6UiBbQ== identity.aes
```

Internal tests
--------------
    $ ./test/test_cli.sh

Building a standalone executable
---

The escript built above requires an Erlang distribution on the target system to run.

It's also possible to package everything into a single executable using Warp (https://github.com/dgiagio/warp/tree/master/examples/erlang):

    $ ./rebar3 escriptize

    $ ./rebar3 release

    $ warp-packer -a linux-x64 -i _build/default/rel/aesophia_cli -e launch -o warp/aesophia_cli

The standalone executable warp/aesophia_cli can now be copied to an x86 linux system without Erlang installed.

Warp also supports windows x86, but this is not tested.

Warp currently does not support arm64.