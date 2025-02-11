
## **Passaggi:**
1. **Installazione di NGINX**
2. **Configurazione del Load Balancer con hashing su GameID**
3. **Riavvio e verifica**

---

### **1. Installare NGINX sul nodo**
Accedi al container e installa NGINX:
```bash
apt update && apt install -y nginx
```
Oppure, se usi Alpine Linux nei container:
```bash
apk add nginx
```
Avvia il servizio:
```bash
service nginx start
```

---

### **2. Configurare il Load Balancer**
Modifica il file di configurazione di NGINX (di solito in `/etc/nginx/nginx.conf` o `/etc/nginx/conf.d/default.conf`).

Aggiungi la configurazione:
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

**Spiegazione:**
- `hash $arg_GameID consistent;` → L'hashing su `GameID` assicura che tutte le richieste di una partita vadano sempre allo stesso nodo.
- I tre server Erlang ascoltano su `8080` (modifica la porta se serve).
- Forwardiamo gli header per preservare l'IP originale del client.

---

### **3. Riavviare e Testare**
Riavvia NGINX per applicare le modifiche:
```bash
service nginx restart
```
Oppure:
```bash
nginx -s reload
```

**Test con `curl` (simulando diversi `GameID`):**
```bash
curl "http://10.2.1.27/?GameID=12345"
curl "http://10.2.1.27/?GameID=67890"
```
Ogni `GameID` verrò gestito sempre allo stesso nodo.

---