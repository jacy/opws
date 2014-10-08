sudo mkdir -p testbin
sudo chown -R jacy:jacy testbin
erl -pa ebin -make
erl -P 262143 -pa ebin  testbin --setcookie donotbesameasrealgame -sname distributed_test -mnesia dump_log_write_threshold 50000 -mnesia dc_dump_limit 40
