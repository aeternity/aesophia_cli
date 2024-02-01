#! /bin/bash

rm -rf _build
mkdir -p all_versions
for VSN in `git tag --list`; do
  echo "Compiling ${VSN}"
  git reset --hard origin/master
  git checkout ${VSN}
  ./rebar3 escriptize
  cp aesophia_cli all_versions/aesophia_cli-${VSN}
done

