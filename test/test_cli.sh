#!/usr/bin/env bash

TMPFILE=`mktemp`
STATUS=0

## Compile
./aesophia_cli test/contracts/identity.aes -o ${TMPFILE}.aeb
if [ ! -f ${TMPFILE}.aeb ]; then
    echo -e "Test FAILED: compile\\n"
    STATUS=1
else
    echo -e "Test PASSED: compile\\n"
fi

## Compile
./aesophia_cli test/contracts/identity.aes -b aevm -o ${TMPFILE}.aeb
if [ ! -f ${TMPFILE}.aeb ]; then
    echo -e "Test FAILED: compile\\n"
    STATUS=1
else
    echo -e "Test PASSED: compile\\n"
fi

## Compile
rm -f ${TMPFILE}.aeb
./aesophia_cli test/contracts/include.aes -o ${TMPFILE}.aeb
if [ ! -f ${TMPFILE}.aeb ]; then
    echo -e "Test FAILED: compile\\n"
    STATUS=1
else
    echo -e "Test PASSED: compile\\n"
fi

## Compile
rm -f ${TMPFILE}.aeb
./aesophia_cli test/contracts/include.aes -i test/contracts/ -o ${TMPFILE}.aeb
if [ ! -f ${TMPFILE}.aeb ]; then
    echo -e "Test FAILED: compile\\n"
    STATUS=1
else
    echo -e "Test PASSED: compile\\n"
fi

## Create ACI-stub
./aesophia_cli --create_stub_aci test/contracts/identity.aes -o ${TMPFILE}.aci_stub
if [ ! -f ${TMPFILE}.aci_stub ]; then
    echo -e "Test FAILED: create aci stub\\n"
    STATUS=1
else
    echo -e "Test PASSED: create aci stub\\n"
fi

## Create ACI-stub
rm -f ${TMPFILE}.aci_stub
./aesophia_cli --create_stub_aci test/contracts/include.aes -o ${TMPFILE}.aci_stub
if [ ! -f ${TMPFILE}.aci_stub ]; then
    echo -e "Test FAILED: create aci stub\\n"
    STATUS=1
else
    echo -e "Test PASSED: create aci stub\\n"
fi


## Create ACI-json
./aesophia_cli --create_json_aci test/contracts/identity.aes -o ${TMPFILE}.aci_json
if [ ! -f ${TMPFILE}.aci_json ]; then
    echo -e "Test FAILED: create aci json\\n"
    STATUS=1
else
    echo -e "Test PASSED: create aci json\\n"
fi

## Create calldata
./aesophia_cli --create_calldata test/contracts/identity.aes --call "main_(42)" -o ${TMPFILE}.calldata1
if [ ! -f ${TMPFILE}.calldata1 ]; then
    echo -e "Test FAILED: create calldata 1\\n"
    STATUS=1
else
    echo -e "Test PASSED: create calldata 1\\n"
fi

## Create calldata
./aesophia_cli --create_calldata test/contracts/identity.aes --call "init()" -o ${TMPFILE}.calldata2
if [ ! -f ${TMPFILE}.calldata2 ]; then
    echo -e "Test FAILED: create calldata 2\\n"
    STATUS=1
else
    echo -e "Test PASSED: create calldata 2\\n"
fi

## Create calldata
./aesophia_cli --create_calldata test/contracts/include.aes --call "foo()" -o ${TMPFILE}.calldata3
if [ ! -f ${TMPFILE}.calldata3 ]; then
    echo -e "Test FAILED: create calldata 3\\n"
    STATUS=1
else
    echo -e "Test PASSED: create calldata 3\\n"
fi

## Decode data
RES=`./aesophia_cli -b aevm --decode_data cb_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACr8s/aY --decode_type int`
echo -e "${RES}"
RES=`echo "${RES}" | tail -n1`
if [ "${RES}" != "42" ]; then
    echo -e "Test FAILED: decode data\\n"
    STATUS=1
else
    echo -e "Test PASSED: decode data\\n"
fi

## Decode data
RES=`./aesophia_cli -b aevm test/contracts/identity.aes --call_result cb_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACr8s/aY --call_result_fun main_`
echo -e "${RES}"
RES=`echo "${RES}" | tail -n1`
if [ "${RES}" != "42" ]; then
    echo -e "Test FAILED: decode call result"
    STATUS=1
else
    echo -e "Test PASSED: decode call result"
fi

rm -rf ${TMPFILE} ${TMPFILE}.*

exit ${STATUS}
