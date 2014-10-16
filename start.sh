#Param1=nodeName, Param2=init script

NODE_NAME="$1@127.0.0.1"
echo "	Start node: $NODE_NAME"

COOKIE="jacygames"
MNESIA_DIR="/mnesia/$NODE_NAME"
LOG_DIR="/var/log/erlang"

echo "	Mnesia storage dir: $MNESIA_DIR"
sudo mkdir -p $MNESIA_DIR
sudo chown -R jacy:jacy $MNESIA_DIR

sudo mkdir -p $LOG_DIR
sudo chown -R jacy:jacy $LOG_DIR

erl -pa ebin  deps/*/ebin  -name $NODE_NAME -setcookie $COOKIE -mnesia dir '"'$MNESIA_DIR'"' -s $2 -detached