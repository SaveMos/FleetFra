import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.springframework.boot.test.context.SpringBootTest;


@SpringBootTest(classes = FleetFraChinExecution.class)
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)

public class FleetFraChinExecutionTest {
    private static final String GAME_ID = FleetFraChinExecution.generateRandomString(20);
    private static final String PLAYER1_ID = "player1";
    private static final String PLAYER2_ID = "player2";
    private static final String SERVER_URL = "http://10.2.1.30:8080/erl";

    @Test
    @Order(1)
    void testStartGame() throws Exception {
        String requestJson = FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER1_ID, 0, 0);
        String responseJson =  FleetFraChinExecution.sendPostRequest(SERVER_URL, requestJson);
        assertNotNull(responseJson);
        assertEquals("{\"message\":\"ERROR: Game not found\"}", responseJson);

        requestJson = FleetFraChinExecution.createStartGameRequest(GAME_ID, PLAYER1_ID, PLAYER2_ID);
        responseJson =  FleetFraChinExecution.sendPostRequest(SERVER_URL, requestJson);
        assertNotNull(responseJson);
        assertEquals(responseJson, "{\"message\":\"OK: Game started\"}");
    }

    @Test
    @Order(2)
    void testInvalidMoves() throws Exception {
        String moveJson = FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER1_ID, 0, -1);
        String responseJson =  FleetFraChinExecution.sendPostRequest(SERVER_URL, moveJson);
        assertNotNull(responseJson);
        assertEquals(responseJson, "{\"message\":\"INVALID MOVE: Out of bound coordinates\"}");

        moveJson = FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER1_ID, -1, 0);
        responseJson =  FleetFraChinExecution.sendPostRequest(SERVER_URL, moveJson);
        assertNotNull(responseJson);
        assertEquals(responseJson, "{\"message\":\"INVALID MOVE: Out of bound coordinates\"}");

        moveJson = FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER1_ID, 10, 0);
        responseJson =  FleetFraChinExecution.sendPostRequest(SERVER_URL, moveJson);
        assertNotNull(responseJson);
        assertEquals(responseJson, "{\"message\":\"INVALID MOVE: Out of bound coordinates\"}");

        moveJson = FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER1_ID, 0, 10);
        responseJson =  FleetFraChinExecution.sendPostRequest(SERVER_URL, moveJson);
        assertNotNull(responseJson);
        assertEquals(responseJson, "{\"message\":\"INVALID MOVE: Out of bound coordinates\"}");

        moveJson = FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER1_ID, 10, 10);
        responseJson =  FleetFraChinExecution.sendPostRequest(SERVER_URL, moveJson);
        assertNotNull(responseJson);
        assertEquals(responseJson, "{\"message\":\"INVALID MOVE: Out of bound coordinates\"}");

        moveJson = FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER1_ID, -1, -1);
        responseJson =  FleetFraChinExecution.sendPostRequest(SERVER_URL, moveJson);
        assertNotNull(responseJson);
        assertEquals(responseJson, "{\"message\":\"INVALID MOVE: Out of bound coordinates\"}");
    }

    @Test
    @Order(3)
    void testValidMove() throws Exception {
        String moveJson = FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER1_ID, 0, 0);
        String responseJson =  FleetFraChinExecution.sendPostRequest(SERVER_URL, moveJson);
        assertNotNull(responseJson);
        assertEquals("{\"message\":\"OK: Move accepted\"}", responseJson);

        moveJson = FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER2_ID, 0, 0);
        responseJson =  FleetFraChinExecution.sendPostRequest(SERVER_URL, moveJson);
        assertNotNull(responseJson);
        assertEquals("{\"message\":\"OK: Move accepted\"}", responseJson);

        moveJson = FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER1_ID, 1, 0);
        responseJson =  FleetFraChinExecution.sendPostRequest(SERVER_URL, moveJson);
        assertNotNull(responseJson);
        assertEquals("{\"message\":\"OK: Move accepted\"}", responseJson);

        moveJson = FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER2_ID, 1, 0);
        responseJson =  FleetFraChinExecution.sendPostRequest(SERVER_URL, moveJson);
        assertNotNull(responseJson);
        assertEquals("{\"message\":\"OK: Move accepted\"}", responseJson);

        moveJson = FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER1_ID, 2, 0);
        responseJson =  FleetFraChinExecution.sendPostRequest(SERVER_URL, moveJson);
        assertNotNull(responseJson);
        assertEquals("{\"message\":\"OK: Move accepted\"}", responseJson);
    }

    @Test
    @Order(4)
    void testTurnError() throws Exception {
        String moveJson = FleetFraChinExecution.createMakeMoveRequest(GAME_ID, PLAYER1_ID, 1, 1);
        String responseJson = FleetFraChinExecution.sendPostRequest(SERVER_URL, moveJson);
        assertNotNull(responseJson);
        assertEquals("{\"message\":\"TURN ERROR: Not your turn\"}", responseJson);
    }

    @Test
    @Order(5)
    void testInvalidPlayer() throws Exception {
        String moveJson = FleetFraChinExecution.createMakeMoveRequest(GAME_ID, "fake_player", 1, 1);
        String responseJson = FleetFraChinExecution.sendPostRequest(SERVER_URL, moveJson);
        assertNotNull(responseJson);
        assertEquals("{\"message\":\"ERROR: Player not found\"}", responseJson);
    }

    @Test
    @Order(6)
    void testInvalidGame() throws Exception {
        String moveJson = FleetFraChinExecution.createMakeMoveRequest("fake_game", PLAYER1_ID, 1, 1);
        String responseJson = FleetFraChinExecution.sendPostRequest(SERVER_URL, moveJson);
        assertNotNull(responseJson);
        assertEquals("{\"message\":\"ERROR: Game not found\"}", responseJson);
    }


}
