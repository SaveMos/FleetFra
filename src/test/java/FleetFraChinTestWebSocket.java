import static org.junit.jupiter.api.Assertions.*;

import org.java_websocket.client.WebSocketClient;
import org.java_websocket.handshake.ServerHandshake;
import org.junit.jupiter.api.*;
import org.springframework.boot.test.context.SpringBootTest;

import java.net.URI;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

/**
 * WebSocket test suite for FleetFraChin.
 * This test suite verifies WebSocket communication, game initialization, move validation,
 * turn handling, and error responses.
 */
@SpringBootTest(classes = FleetFraChinExecution.class)
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class FleetFraChinTestWebSocket {

    // Constants for game and player identifiers
    private static final String GAME_ID = FleetFraChinExecution.generateRandomString(20);
    private static final String PLAYER1_ID = "player1";
    private static final String PLAYER2_ID = "player2";
    private static final String SERVER_URL = "ws://10.2.1.30:8080/ws";

    private WebSocketClient client;
    private CountDownLatch latch;
    private String receivedMessage;

    /**
     * Initializes a WebSocket client before each test.
     * Establishes a connection and ensures WebSocket is open.
     */
    @BeforeEach
    public void setup() throws Exception {
        latch = new CountDownLatch(1);
        receivedMessage = null;

        client = new WebSocketClient(new URI(SERVER_URL)) {
            @Override
            public void onOpen(ServerHandshake handshake) {
                System.out.println("✅ WebSocket connected");
            }

            @Override
            public void onMessage(String message) {
                System.out.println("📩 Received: " + message);
                receivedMessage = message.replace("\\\"", "\""); // Normalize JSON format
                latch.countDown();
            }

            @Override
            public void onClose(int code, String reason, boolean remote) {
                System.out.println("❌ WebSocket closed. Reason: " + reason);
            }

            @Override
            public void onError(Exception ex) {
                System.err.println("⚠️ WebSocket error: " + ex.getMessage());
            }
        };

        client.connectBlocking();
        assertTrue(client.isOpen(), "WebSocket connection is not active!");
    }

    /**
     * Sends a request and waits for the expected response.
     *
     * @param requestJson      The JSON request payload.
     * @param expectedResponse The expected JSON response.
     * @throws Exception if there is a timeout or assertion failure.
     */
    private void sendAndAwaitResponse(String requestJson, String expectedResponse) throws Exception {
        latch = new CountDownLatch(1); // Reset the latch for each request
        client.send(requestJson);
        boolean messageReceived = latch.await(10, TimeUnit.SECONDS); // Wait max 10s

        assertTrue(messageReceived, "❌ No response from server!");
        assertNotNull(receivedMessage, "❌ No message received!");
        assertEquals(expectedResponse, receivedMessage);
    }

    /**
     * Test to start a game and verify a successful response.
     */
    @Test
    @Order(1)
    void testStartGame() throws Exception {
        sendAndAwaitResponse(
                FleetFraChinExecution.createStartGameRequest(GAME_ID, PLAYER1_ID, PLAYER2_ID),
                "{\"message\":\"OK: Game started\"}"
        );
    }

    /**
     * Test invalid move coordinates and expect an "Out of bound coordinates" response.
     */
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

    /**
     * Test valid moves for both players and expect success responses.
     */
    @Test
    @Order(3)
    void testValidMove() throws Exception {
        sendAndAwaitResponse(FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER1_ID, 0, 0), "{\"message\":\"OK: Move accepted\"}");
        sendAndAwaitResponse(FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER2_ID, 0, 0), "{\"message\":\"OK: Move accepted\"}");
        sendAndAwaitResponse(FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER1_ID, 1, 0), "{\"message\":\"OK: Move accepted\"}");
        sendAndAwaitResponse(FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER2_ID, 1, 0), "{\"message\":\"OK: Move accepted\"}");
        sendAndAwaitResponse(FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER1_ID, 2, 0), "{\"message\":\"OK: Move accepted\"}");
    }

    /**
     * Test turn validation error by attempting to move when it's not the player's turn.
     */
    @Test
    @Order(4)
    void testTurnError() throws Exception {
        sendAndAwaitResponse(
                FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER1_ID, 1, 1),
                "{\"message\":\"TURN ERROR: Not your turn\"}"
        );
    }

    /**
     * Test an invalid player attempting to make a move.
     */
    @Test
    @Order(5)
    void testInvalidPlayer() throws Exception {
        sendAndAwaitResponse(
                FleetFraChinExecution.createMakeMoveRequest(GAME_ID, "fake_player", 1, 1),
                "{\"message\":\"ERROR: Player not found\"}"
        );
    }

    /**
     * Test an invalid game identifier and expect an error response.
     */
    @Test
    @Order(6)
    void testInvalidGame() throws Exception {
        sendAndAwaitResponse(
                FleetFraChinExecution.createMakeMoveRequest("fake_game", PLAYER1_ID, 1, 1),
                "{\"message\":\"ERROR: Game not found\"}"
        );
    }

    /**
     * Cleanup method executed after each test.
     * Ensures WebSocket connection is closed properly.
     */
    @AfterEach
    public void cleanup() {
        if (client != null && client.isOpen()) {
            client.close();
        }
    }
}
