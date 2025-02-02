import static org.junit.jupiter.api.Assertions.*;

import org.java_websocket.client.WebSocketClient;
import org.java_websocket.handshake.ServerHandshake;
import org.junit.jupiter.api.*;
import org.springframework.boot.test.context.SpringBootTest;

import java.net.URI;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

@SpringBootTest(classes = FleetFraChinExecution.class)
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class FleetFraChinTestWebSocket {

    private static final String GAME_ID = FleetFraChinExecution.generateRandomString(20);
    private static final String PLAYER1_ID = "player1";
    private static final String PLAYER2_ID = "player2";
    private static final String SERVER_URL = "ws://10.2.1.30:8080/ws";

    private WebSocketClient client;
    private CountDownLatch latch;
    private String receivedMessage;

    @BeforeEach
    public void setup() throws Exception {
        latch = new CountDownLatch(1);
        receivedMessage = null;

        client = new WebSocketClient(new URI(SERVER_URL)) {
            @Override
            public void onOpen(ServerHandshake handshake) {
                System.out.println("✅ WebSocket connesso");
            }

            @Override
            public void onMessage(String message) {
                System.out.println("📩 Ricevuto: " + message);
                receivedMessage = message;
                latch.countDown();
            }

            @Override
            public void onClose(int code, String reason, boolean remote) {
                System.out.println("❌ WebSocket chiuso. Motivo: " + reason);
            }

            @Override
            public void onError(Exception ex) {
                System.err.println("⚠️ Errore WebSocket: " + ex.getMessage());
            }
        };

        client.connectBlocking();
        assertTrue(client.isOpen(), "La connessione WebSocket non è attiva!");
    }

    private void sendAndAwaitResponse(String requestJson, String expectedResponse) throws Exception {
        latch = new CountDownLatch(1); // Resetta il latch per ogni richiesta
        client.send(requestJson);
        boolean messageReceived = latch.await(5, TimeUnit.SECONDS); // Attendi max 5s

        assertTrue(messageReceived, "❌ Nessuna risposta dal server!");
        assertNotNull(receivedMessage, "❌ Nessun messaggio ricevuto!");
        assertEquals(expectedResponse, receivedMessage);
    }

    @Test
    @Order(1)
    void testStartGame() throws Exception {
        sendAndAwaitResponse(
                FleetFraChinExecution.createStartGameRequest(GAME_ID, PLAYER1_ID, PLAYER2_ID),
                "{\"message\":\"OK: Game started\"}"
        );
    }

    @Test
    @Order(2)
    void testInvalidMoves() throws Exception {
        String expectedResponse = "{\"message\":\"INVALID MOVE: Out of bound coordinates\"}";

        sendAndAwaitResponse(FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER1_ID, 0, -1), expectedResponse);
        sendAndAwaitResponse(FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER1_ID, -1, 0), expectedResponse);
        sendAndAwaitResponse(FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER1_ID, 10, 0), expectedResponse);
        sendAndAwaitResponse(FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER1_ID, 0, 10), expectedResponse);
        sendAndAwaitResponse(FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER1_ID, 10, 10), expectedResponse);
        sendAndAwaitResponse(FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER1_ID, -1, -1), expectedResponse);
    }

    @Test
    @Order(3)
    void testValidMove() throws Exception {
        sendAndAwaitResponse(FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER1_ID, 0, 0), "{\"message\":\"OK: Move accepted\"}");
        sendAndAwaitResponse(FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER2_ID, 0, 0), "{\"message\":\"OK: Move accepted\"}");
        sendAndAwaitResponse(FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER1_ID, 1, 0), "{\"message\":\"OK: Move accepted\"}");
        sendAndAwaitResponse(FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER2_ID, 1, 0), "{\"message\":\"OK: Move accepted\"}");
        sendAndAwaitResponse(FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER1_ID, 2, 0), "{\"message\":\"OK: Move accepted\"}");
    }

    @Test
    @Order(4)
    void testTurnError() throws Exception {
        sendAndAwaitResponse(
                FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER1_ID, 1, 1),
                "{\"message\":\"TURN ERROR: Not your turn\"}"
        );
    }

    @Test
    @Order(5)
    void testInvalidPlayer() throws Exception {
        sendAndAwaitResponse(
                FleetFraChinExecution.createMakeMoveRequest(GAME_ID, "fake_player", 1, 1),
                "{\"message\":\"ERROR: Player not found\"}"
        );
    }

    @Test
    @Order(6)
    void testInvalidGame() throws Exception {
        sendAndAwaitResponse(
                FleetFraChinExecution.createMakeMoveRequest("fake_game", PLAYER1_ID, 1, 1),
                "{\"message\":\"ERROR: Game not found\"}"
        );
    }

    @AfterEach
    public void cleanup() {
        if (client != null && client.isOpen()) {
            client.close();
        }
    }
}
