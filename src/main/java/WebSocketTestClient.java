import org.java_websocket.client.WebSocketClient;
import org.java_websocket.handshake.ServerHandshake;

import java.net.URI;
import java.net.URISyntaxException;

public class WebSocketTestClient {
    private static final String SERVER_URL = "ws://10.2.1.30:8080/ws";

    public static void main(String[] args) {

        try {
            WebSocketClient client = new WebSocketClient(new URI(SERVER_URL)) {
                @Override
                public void onOpen(ServerHandshake handshakedata) {
                    System.out.println("WebSocket connected successfully!");
                    send("{\"message\": \"Test connection\"}"); // Invio messaggio di test
                }

                @Override
                public void onMessage(String message) {
                    System.out.println("Received message: " + message);
                }

                @Override
                public void onClose(int code, String reason, boolean remote) {
                    System.out.println("WebSocket closed: " + reason);
                }

                @Override
                public void onError(Exception ex) {
                    System.err.println("WebSocket error: " + ex.getMessage());
                    ex.printStackTrace();
                }
            };

            client.connectBlocking();  // Aspetta la connessione
            Thread.sleep(3000);  // Attendere qualche secondo per ricevere risposte
            client.close();  // Chiude la connessione

        } catch (URISyntaxException | InterruptedException e) {
            e.printStackTrace();
        }
    }
}
