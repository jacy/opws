NODE_NAME=log_roller_server
rebar get-deps
rebar clean compile
cd deps/ilog
make
kill -9 $(ps aux | grep $NODE_NAME | awk '{print $2}')
erl -pa ebin  ../*/ebin -setcookie $1 -sname $NODE_NAME -s log_roller_server start $2
cd -