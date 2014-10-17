sudo mkdir -p testbin
sudo chown -R jacy:jacy testbin
erl -pa ebin -make

#http://www.lognormal.com/blog/2012/09/27/linux-tcpip-tuning/
#Modify TIME_WAIT time so address can be quickly reuse.
echo 1 | sudo tee /proc/sys/net/ipv4/tcp_fin_timeout
sudo sysctl -w net.ipv4.tcp_fin_timeout=1
#Modify ip port range so it can handle more TCP sessions at a time
sudo sysctl -w net.ipv4.ip_local_port_range="18000 65535"
erl -P 26214300 -pa ebin  testbin --setcookie donotbesameasrealgame -sname distributed_test -mnesia dump_log_write_threshold 50000 -mnesia dc_dump_limit 40  -env ERL_MAX_ETS_TABLES 500000 +t 1048576