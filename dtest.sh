sudo mkdir -p testbin
sudo chown -R jacy:jacy testbin
erl -pa ebin -make

#http://www.lognormal.com/blog/2012/09/27/linux-tcpip-tuning/
#Modify TIME_WAIT time so address can be quickly reuse.
echo 1 | sudo tee /proc/sys/net/ipv4/tcp_fin_timeout
sudo sysctl -w net.ipv4.tcp_fin_timeout=1

# Modify local ip port range so as to increase max tcp connections allowed to be created in tcp client side to one remote port.
# Each TCP/IP packet has basically four fields for addressing; these are:
# source_ip,source_port,destination_ip,destination_port
# if simulate bot in same machine, the maximum number of connections can have to an given host port is 64K.
# However, multiple clients can each have up to 64K connections to some server's port.
# So the real limit is file descriptors. Each individual socket connection is given a file descriptor, so the limit is really the number of 
# file descriptors that the system has been configured to allow and resources to handle. The maximum limit is typically up over 300K

sudo sysctl -w net.ipv4.ip_local_port_range="10000 65535"

sudo /etc/init.d/networking restart

erl +P 4194304 -pa ebin  testbin --setcookie donotbesameasrealgame -sname distributed_test -mnesia dump_log_write_threshold 50000 -mnesia dc_dump_limit 40  -env ERL_MAX_ETS_TABLES 500000 +t 1048576