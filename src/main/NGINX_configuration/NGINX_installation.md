
---

## **Steps:**
1. **Install NGINX**
2. **Configure the Load Balancer with hashing on GameID**
3. **Restart and Verify**

---

### **1. Install NGINX on the Node**
Access the container and install NGINX:
```bash
apt update && apt install -y nginx
```
Alternatively, if you're using Alpine Linux in containers:
```bash
apk add nginx
```
Start the service:
```bash
service nginx start
```

---

### **2. Configure the Load Balancer**
Edit the NGINX configuration file (usually located at `/etc/nginx/nginx.conf` or `/etc/nginx/conf.d/default.conf`).

Add the following configuration:
```nginx
user www-data;
worker_processes auto;
pid /run/nginx.pid;
include /etc/nginx/modules-enabled/*.conf;

events {
    worker_connections 768;
}

http {
    ##
    # Basic Settings
    ##
    sendfile on;
    tcp_nopush on;
    types_hash_max_size 2048;
    include /etc/nginx/mime.types;
    default_type application/octet-stream;

    ##
    # SSL Settings
    ##
    ssl_protocols TLSv1 TLSv1.1 TLSv1.2 TLSv1.3;
    ssl_prefer_server_ciphers on;

    ##
    # Logging Settings
    ##
    access_log /var/log/nginx/access.log;
    error_log /var/log/nginx/error.log;

    ##
    # WebSocket Load Balancer Configuration
    ##
    upstream erlang_servers {
        hash $arg_GameID consistent;  # Balancing based on the GameID of the match.
        server 10.2.1.30:8080;
        server 10.2.1.29:8080;
        server 10.2.1.28:8080;
    }

    server {
        listen 8080;
        server_name enac_oid_balancer;

        location /ws {
            proxy_pass http://erlang_servers;
            proxy_http_version 1.1;
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection "Upgrade";
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        }
    }

    ##
    # Virtual Host Configs
    ##
    include /etc/nginx/conf.d/*.conf;
    include /etc/nginx/sites-enabled/*;
}
```

**Explanation:**
- `hash $arg_GameID consistent;` → Hashing on `GameID` ensures that all requests from a match are always routed to the same node.
- The three Erlang servers are listening on `8080` (modify the port if needed).
- Forwarding headers to preserve the original client IP.

---

### **3. Restart and Test**
Restart NGINX to apply the changes:
```bash
service nginx restart
```
Alternatively:
```bash
nginx -s reload
```

**Test with `curl` (simulating different `GameID`s):**
```bash
curl "http://10.2.1.27/?GameID=12345"
curl "http://10.2.1.27/?GameID=67890"
```
Each `GameID` will always be handled by the same node.

--- 
