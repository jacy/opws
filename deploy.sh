echo "Compiling...."
erl -make
echo '==================================='
echo 'Start Mnesia Master: m1@127.0.0.1'
./start.sh m1 "cluster mnesia_master"
echo '==================================='
sleep 2
echo 'Start Lobby: lobby@127.0.0.1'
./start.sh lobby "cluster setup_lobby m1@127.0.0.1"
echo '==================================='
echo 'Start Game: game@127.0.0.1'
./start.sh game "cluster setup_games m1@127.0.0.1"
echo '==================================='