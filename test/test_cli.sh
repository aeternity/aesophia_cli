#!/usr/bin/env bash

TMPFILE=`mktemp`
STATUS=0

test_chk () {
  local res=$1
  local msg=$2
  if [ ${res} -ne 0 ]; then
      echo -e "Test FAILED: ${msg}\\n"
      STATUS=1
  else
      echo -e "Test PASSED: ${msg}\\n"
  fi
}

## Compile
./aesophia_cli test/contracts/identity.aes -o ${TMPFILE}.aeb
[ -f ${TMPFILE}.aeb ]
test_chk $? "create compile 1"

## Compile
./aesophia_cli test/contracts/identity.aes -o ${TMPFILE}.aeb
[ -f ${TMPFILE}.aeb ]
test_chk $? "create compile 2"

## Compile
rm -f ${TMPFILE}.aeb
./aesophia_cli test/contracts/include.aes -o ${TMPFILE}.aeb
[ -f ${TMPFILE}.aeb ]
test_chk $? "create compile 3"

## Compile
rm -f ${TMPFILE}.aeb
./aesophia_cli test/contracts/include.aes -i test/contracts/ -o ${TMPFILE}.aeb
[ -f ${TMPFILE}.aeb ]
test_chk $? "create compile 4"

## Create ACI-stub
./aesophia_cli --create_stub_aci test/contracts/identity.aes -o ${TMPFILE}.aci_stub
[ -f ${TMPFILE}.aci_stub ]
test_chk $? "create aci stub 1"

## Create ACI-stub
rm -f ${TMPFILE}.aci_stub
./aesophia_cli --create_stub_aci test/contracts/include.aes -o ${TMPFILE}.aci_stub
[ -f ${TMPFILE}.aci_stub ]
test_chk $? "create aci stub 2"

## Create ACI-json
./aesophia_cli --create_json_aci test/contracts/identity.aes -o ${TMPFILE}.aci_json
[ -f ${TMPFILE}.aci_json ]
test_chk $? "create aci json"

## Create calldata
./aesophia_cli --create_calldata test/contracts/identity.aes --call "main_(42)" -o ${TMPFILE}.calldata1
[ -f ${TMPFILE}.calldata1 ]
test_chk $? "create calldata 1"

## Create calldata
./aesophia_cli --create_calldata test/contracts/identity.aes --call "init()" -o ${TMPFILE}.calldata2
[ -f ${TMPFILE}.calldata2 ]
test_chk $? "create calldata 2"

## Create calldata
./aesophia_cli --create_calldata test/contracts/include.aes --call "foo()" -o ${TMPFILE}.calldata3
[ -f ${TMPFILE}.calldata3 ]
test_chk $? "create calldata 3"

## Decode calldata
./aesophia_cli --decode_calldata cb_KxG3+3bAG1StlAV3 --calldata_fun main_ test/contracts/identity.aes
test_chk $? "decode calldata 1"

rm -rf ${TMPFILE} ${TMPFILE}.*

exit ${STATUS}
