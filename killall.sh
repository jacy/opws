kill -9 $(ps aux | grep @127.0.0.1 | awk '{print $2}')
