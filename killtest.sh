kill -9 $(ps aux | grep distributed | awk '{print $2}')
