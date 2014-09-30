sudo mkdir -p testbin
sudo chown -R jacy:jacy testbin
erl -pa ebin -make
erl -pa ebin  testbin --setcookie donotbesameasrealgame -eval "io:format(\"Running tests,please wait...~n\",[]),test:all(),init:stop()."
