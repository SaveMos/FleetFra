import static org.junit.jupiter.api.Assertions.*;

import org.java_websocket.client.WebSocketClient;
import org.java_websocket.handshake.ServerHandshake;
import org.junit.jupiter.api.*;
import org.springframework.boot.test.context.SpringBootTest;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Random;
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

    private String sendAndAwaitResponse(String requestJson) throws Exception {
        latch = new CountDownLatch(1); // Reset the latch for each request
        client.send(requestJson);
        boolean messageReceived = latch.await(10, TimeUnit.SECONDS); // Wait max 10s
        return receivedMessage;
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
     * Test an entire game.
     */
    @Test
    @Order(7)
    void testFullGame() throws Exception {
        String gameID = FleetFraChinExecution.generateRandomString(20);
        String player1FinalState = "";
        String player2FinalState = "";
        String currentPlayer;
        String requestJson, responseJson;

        // STARTING GAME TEST
        sendAndAwaitResponse(
                FleetFraChinExecution.createStartGameRequest(gameID, PLAYER1_ID, PLAYER2_ID),
                "{\"message\":\"OK: Game started\"}"
        );

        // SIMULATED MATCH TEST
        // Game loop: alternate turns
        Random rand = new Random();
        List<Map<String, Integer>> player1Battlefield = FleetFraChinExecution.generateBattlefield();
        List<Map<String, Integer>> player2Battlefield = FleetFraChinExecution.generateBattlefield();

        // List the ship positions for player2 (player1 has to sink them)
        List<Map<String, Integer>> player2ShipPositions = new ArrayList<>();
        for (Map<String, Integer> cell : player2Battlefield) {
            if (cell.get("value") == 1) {
                player2ShipPositions.add(cell);  // Collect the cells where player2's ships are located
            }
        }

        int turn = 0;
        while (true) {
            currentPlayer = (turn % 2 == 0) ? PLAYER1_ID : PLAYER2_ID;
            List<Map<String, Integer>> currentPlayerBattlefield = (turn % 2 == 0) ? player1Battlefield : player2Battlefield;
            List<Map<String, Integer>> opponentBattlefield = (turn % 2 == 0) ? player2Battlefield : player1Battlefield;

            int row = -1, col = -1;

            if (currentPlayer.equals(PLAYER1_ID) && !player2ShipPositions.isEmpty()) {
                // Player1 hits positions of player2's ships without missing
                Map<String, Integer> targetCell = player2ShipPositions.get(0);
                row = targetCell.get("row");
                col = targetCell.get("col");

                // Remove the hit ship from the list
                player2ShipPositions.removeFirst();
            } else {
                // Player2 shoots randomly
                row = rand.nextInt(10);
                col = rand.nextInt(10);
            }

            requestJson = FleetFraChinExecution.createMakeMoveRequest(gameID, currentPlayer, row, col);
            responseJson = sendAndAwaitResponse(requestJson);

            switch (responseJson) {
                case "{\"message\":\"OK: Move accepted\"}" -> {
                    System.out.println(currentPlayer + " has played.");
                }
                // Check if the game has ended (e.g., "VICTORY" or "DEFEAT" directly from the response)
                case "{\"message\":\"VICTORY\"}" -> {
                    if (currentPlayer.equals(PLAYER1_ID)) {
                        player1FinalState = "VICTORY";  // Player1 won
                    } else {
                        player2FinalState = "VICTORY";  // Player2 won
                    }
                }
                case "{\"message\":\"DEFEAT\"}" -> {
                    if (currentPlayer.equals(PLAYER1_ID)) {
                        player1FinalState = "DEFEAT";  // Player1 lost
                    } else {
                        player2FinalState = "DEFEAT";  // Player2 lost
                    }
                }
            }

            if ((player1FinalState.equals("VICTORY") || player1FinalState.equals("DEFEAT")) &&
                    (player2FinalState.equals("VICTORY") || player2FinalState.equals("DEFEAT"))){
                break;  // End the game if both players have a final state (either victory or defeat)
            }

            turn++; // Proceed to the next turn
        }


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
