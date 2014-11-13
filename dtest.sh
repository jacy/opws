COOKIE="donotbesameasrealgame"

sudo mkdir -p testbin
sudo chown -R jacy:jacy testbin

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

#dc_dump_limit
	#This variable controls how often disc_copies tables are dumped from 
	#memory. The default value is 4, which means if the size of the log is greater 
	#than the size of table / 4, then a dump occurs. To make table dumps happen
	#more often, increase the value. 
	
rebar compile
erl -pa ebin -make  #compile to testbin
erl +P 4194304 -pa ebin  testbin deps/*/ebin -setcookie $COOKIE -sname distributed_test -env ERL_MAX_ETS_TABLES 1000000 +t 1048576
