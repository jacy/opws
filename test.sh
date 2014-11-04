sudo mkdir -p testbin
sudo chown -R jacy:jacy testbin
erl -pa ebin -make
erl -DTEST -pa ebin  testbin deps/*/ebin  -env ERL_MAX_ETS_TABLES 100000 -sname unittest --setcookie donotbesameasrealgame -eval "eunit:test(test)." -s init stop