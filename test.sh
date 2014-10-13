sudo mkdir -p testbin
sudo chown -R jacy:jacy testbin
erl -pa ebin -make
erl -pa ebin  testbin --setcookie donotbesameasrealgame -s test test
