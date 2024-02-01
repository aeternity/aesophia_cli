#! /bin/bash

rm -rf _build
mkdir -p all_versions
for VSN in `git tag --list`; do
  if [ -d priv/bin/${VSN} ]; then
    echo "Saving as priv/bin/${VSN}/aesophia_cli"
    cp all_versions/aesophia_cli-${VSN} priv/bin/${VSN}/aesophia_cli
  fi
done
