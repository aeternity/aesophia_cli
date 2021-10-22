sophia_vsn=$1

[ -z $sophia_vsn ] && echo -e "USAGE: ./update.sh SOPHIA_VSN\nEXAMPLE: ./update.sh 6.0.2" && exit 1

sed -i "s#aesophia.git\", {tag, \".*\"}}}#aesophia.git\", {tag, \"v$sophia_vsn\"}}}#g" rebar.config
sed -i "s#{vsn, \".*\"}#{vsn, \"$sophia_vsn\"}#g" src/aesophia_cli.app.src
./rebar3 upgrade
./rebar3 escriptize
mkdir priv/bin/v$sophia_vsn
cp aesophia_cli priv/bin/v$sophia_vsn/
test/test_cli.sh && echo -e "\e[1;32mDone! Remember to update CHANGELOG.md\e[0m" || echo -e "\e[1;31mSome tests have failed\e[0m"

