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

## Create calldata
./aesophia_cli --create_calldata ${TMPFILE}.aeb --calldata_fun main --calldata_args 42 -o ${TMPFILE}.calldata
if [ ! -f ${TMPFILE}.calldata ]; then
    echo -e "Test FAILED: create calldata\\n"
    STATUS=1
else
    echo -e "Test PASSED: create calldata\\n"
fi

## Decode data
RES=`./aesophia_cli --decode_data cb_AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACr8s/aY --decode_type int`
if [ "${RES}" = "42" ]; then
    echo -e "Test FAILED: decode data"
    STATUS=1
else
    echo -e "Test PASSED: decode data"
fi

rm -rf ${TMPFILE} ${TMPFILE}.*

exit ${STATUS}
