sudo mkdir -p testbin
sudo chown -R jacy:jacy testbin
erl -pa ebin -make
erl -DTEST -pa ebin  testbin  -env ERL_MAX_ETS_TABLES 100000 -sname unittest --setcookie donotbesameasrealgame -mnesia dump_log_write_threshold 50000 -mnesia dc_dump_limit 40 -eval "eunit:test(test)." -s init stop