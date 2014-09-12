COOKIE='jacygames'
NODE_NAME='s2@127.0.0.1'
MNESIA_DIR='/mnesia/'$NODE_NAME

echo 'Mnesia storage dir:'$MNESIA_DIR
sudo mkdir -p $MNESIA_DIR
sudo chown -R jacy:jacy $MNESIA_DIR

erl -make
erl -pa ebin  deps/*/ebin  -name $NODE_NAME -setcookie $COOKIE -mnesia dir '"'$MNESIA_DIR'"'