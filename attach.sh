NODE_NAME="$1@127.0.0.1"
echo "$NODE_NAME"

COOKIE="jacygames"
erl -pa ebin  deps/*/ebin -name attach@127.0.0.1 -setcookie $COOKIE -remsh $NODE_NAME