import static org.junit.jupiter.api.Assertions.*;

import org.java_websocket.client.WebSocketClient;
import org.java_websocket.handshake.ServerHandshake;
import org.junit.jupiter.api.*;
import org.springframework.boot.test.context.SpringBootTest;

import java.net.URI;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

@SpringBootTest(classes = FleetFraChinExecution.class)
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class TestDoubleResponseWebSocket {

    private static final String GAME_ID = FleetFraChinExecution.generateRandomString(20);
    private static final String PLAYER1_ID = "player111";
    private static final String PLAYER2_ID = "player222";
    private static final String SERVER_URL = "ws://10.2.1.30:8080/ws";

    private static WebSocketClient client1;
    private static WebSocketClient client2;
    private static CountDownLatch latch;
    private static String receivedMessage1;
    private static String receivedMessage2;

    // Inizializza le connessioni WebSocket una sola volta in @BeforeEach
    @BeforeAll
    public static void setup() throws Exception {
        latch = new CountDownLatch(2);
        receivedMessage1 = null;
        receivedMessage2 = null;

        // Solo una volta: crea e collega client1 e client2
        if (client1 == null || !client1.isOpen()) {
            client1 = new WebSocketClient(new URI(SERVER_URL + "?game_id=" + GAME_ID + "&player=" + PLAYER1_ID)) {
                @Override
                public void onOpen(ServerHandshake handshake) {
                    System.out.println("📡 Player1 WebSocket connected");
                    latch.countDown();
                }

                @Override
                public void onMessage(String message) {
                    receivedMessage1 = message;
                    System.out.println("📩 Player1 received: " + message);
                    latch.countDown();
                }

                @Override
                public void onClose(int code, String reason, boolean remote) {
                    System.out.println("❌ Player1 WebSocket closed. Reason: " + reason);
                }

                @Override
                public void onError(Exception ex) {
                    System.err.println("⚠️ Player1 WebSocket error: " + ex.getMessage());
                }
            };
            client1.connectBlocking();
        }

        if (client2 == null || !client2.isOpen()) {
            client2 = new WebSocketClient(new URI(SERVER_URL + "?game_id=" + GAME_ID + "&player=" + PLAYER2_ID)) {
                @Override
                public void onOpen(ServerHandshake handshake) {
                    System.out.println("📡 Player2 WebSocket connected");
                    latch.countDown();
                }

                @Override
                public void onMessage(String message) {
                    receivedMessage2 = message;
                    System.out.println("📩 Player2 received: " + message);
                    latch.countDown();
                }

                @Override
                public void onClose(int code, String reason, boolean remote) {
                    System.out.println("❌ Player2 WebSocket closed. Reason: " + reason);
                }

                @Override
                public void onError(Exception ex) {
                    System.err.println("⚠️ Player2 WebSocket error: " + ex.getMessage());
                }
            };
            client2.connectBlocking();
        }
    }

    /**
     * Test to start a game and verify a successful response.
     */
    @Test
    @Order(1)
    void testStartGame() throws Exception {
        List<Map<String, Integer>> player1Battlefield = FleetFraChinExecution.generateBattlefield();
        latch = new CountDownLatch(1);
        String testMessage = FleetFraChinExecution.createStartGameRequestClient(GAME_ID, PLAYER1_ID, player1Battlefield);
        client1.send(testMessage);
        boolean messagesReceived = latch.await(5, TimeUnit.SECONDS);

        assertTrue(messagesReceived, "One or both players did not receive the broadcast message");

        List<Map<String, Integer>> player2Battlefield = FleetFraChinExecution.generateBattlefield();
        latch = new CountDownLatch(1);
        testMessage = FleetFraChinExecution.createStartGameRequestClient(GAME_ID, PLAYER2_ID, player2Battlefield);
        client2.send(testMessage);
        messagesReceived = latch.await(5, TimeUnit.SECONDS);

        assertTrue(messagesReceived, "One or both players did not receive the broadcast message");
    }

    @Test
    @Order(2)
    void testBroadcastMessageToBothPlayers() throws Exception {
        latch = new CountDownLatch(1);
        String testMessage = FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER1_ID, 0, 0);
        client1.send(testMessage);

        boolean messagesReceived = latch.await(10, TimeUnit.SECONDS);

        assertTrue(messagesReceived, "One or both players did not receive the broadcast message");
        //assertEquals(testMessage, receivedMessage2, "Player 2 did not receive the expected message");
    }

    @AfterAll
    public static void cleanup() {
        // Non chiudere i WebSocket qui, poiché li vogliamo riutilizzare nei test
        // Se desideri davvero chiudere le connessioni, farlo solo dopo tutti i test
        if (client1 != null && client1.isOpen()) {
           client1.close();
        }
        if (client2 != null && client2.isOpen()) {
            client2.close();
        }
    }
}

