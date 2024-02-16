#! /bin/bash

if [ ! -d all_versions ]; then
  echo "No folder 'all_versions' found. You need to compile first!"
  exit 1
fi

for VSN in `git tag --list`; do
  if [ -d priv/bin/${VSN} ]; then
    echo "Saving as priv/bin/${VSN}/aesophia_cli"
    cp all_versions/aesophia_cli-${VSN} priv/bin/${VSN}/aesophia_cli
  fi
done
