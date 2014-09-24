rm -rf ebin/*
echo "Compiling...."
erl -pa ebin -make
echo '==================================='
echo 'Set up Mnesia Master: m1@127.0.0.1'
./start.sh m1 "cluster mnesia_master"
echo 'Waitting...'
sleep 2
echo 'Mnesia Master Was Started'
echo '==================================='
echo 'Set up Lobby: lobby@127.0.0.1'
./start.sh lobby "cluster setup_lobby m1@127.0.0.1"
echo '==================================='
echo 'Set up Game: game@127.0.0.1'
./start.sh game "cluster setup_games m1@127.0.0.1"
echo '==================================='