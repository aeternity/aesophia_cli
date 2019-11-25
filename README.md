aesophia_cli
=====

A simple command line interface for the [Sophia compiler](https://github.com/aeternity/aesophia).

Build
-----

    $ ./rebar3 escriptize

Run
---

    $ ./aesophia_cli

```
EXAMPLES:
[compile (for default FATE backend)] :
  aesophia_cli identity.aes -o identity.aeb
[compile (for AEVM)] :
  aesophia_cli identity.aes -b aevm -o identity.aeb
[compile with explicit include path] :
  aesophia_cli identity.aes -i /path/to/include/ -o identity.aeb
[create aci stub] :
  aesophia_cli --create_stub_aci identity.aes
[create aci JSON] :
  aesophia_cli --create_json_aci identity.aes -o identity.json
[create calldata] :
  aesophia_cli --create_calldata identity.aes --call "main(42)"
[decode call result] :
  aesophia_cli identity.aes -b aevm --call_result cb_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACr8s/aY --call_result_fun main
[decode data] :
  aesophia_cli -b aevm --decode_data cb_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACr8s/aY --decode_type int
[validate byte code] :
  aesophia_cli --validate cb_+GpGA6CpNW171TSUfk88PoVv7YslUgxRcOJYKFPRxoGkXArWosC4OZ7+RNZEHwA3ADcAGg6CPwEDP/64F37sADcBBwcBAQCWLwIRRNZEHxFpbml0EbgXfuwRbWFpboIvAIk0LjEuMC1yYzEAXs3cNQ== identity.aes
```

Test
---
    $ ./test/test_cli.sh
