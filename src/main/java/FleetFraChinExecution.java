import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.entity.StringEntity;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.*;

public class FleetFraChinExecution {

    private static final String SERVER_URL = "http://10.2.1.30:8080/erl";

    /**
     * Utility method to send a POST request with a JSON payload.
     * @param url The target URL.
     * @param jsonPayload The JSON string to send as the request body.
     * @return The response as a String.
     * @throws Exception If an error occurs during the request.
     */
    public static String sendPostRequest(String url, String jsonPayload) throws Exception {
        HttpPost postRequest = new HttpPost(url);
        postRequest.setEntity(new StringEntity(jsonPayload, StandardCharsets.UTF_8));
        postRequest.setHeader("Content-Type", "application/json");

        try (CloseableHttpClient client = HttpClients.createDefault()) {
            HttpResponse response = client.execute(postRequest);
            BufferedReader reader = new BufferedReader(new InputStreamReader(response.getEntity().getContent()));
            StringBuilder responseString = new StringBuilder();
            String line;
            while ((line = reader.readLine()) != null) {
                responseString.append(line);
            }
            return responseString.toString();
        }
    }

    /**
     * Creates a request to start a new game.
     * @param gameId The unique game identifier.
     * @param player1 The first player's ID.
     * @param player2 The second player's ID.
     * @return JSON formatted request string.
     * @throws Exception If an error occurs during JSON processing.
     */
    public static String createStartGameRequest(String gameId, String player1, String player2) throws Exception {
        ObjectMapper objectMapper = new ObjectMapper();
        String jsonPayload = objectMapper.writeValueAsString(Map.of(
                "game_id", gameId,
                "type_request", "start_game",
                "player1", player1,
                "player2", player2,
                "player1_battlefield", generateBattlefield(),
                "player2_battlefield", generateBattlefield()
        ));
        return jsonPayload;
    }

    /**
     * Creates a request for making a move.
     * @param gameId The unique game identifier.
     * @param player The player's ID making the move.
     * @param row The row coordinate of the move.
     * @param col The column coordinate of the move.
     * @return JSON formatted request string.
     * @throws Exception If an error occurs during JSON processing.
     */
    public static String createMakeMoveRequest(String gameId, String player, int row, int col) throws Exception {
        ObjectMapper objectMapper = new ObjectMapper();
        String jsonPayload = objectMapper.writeValueAsString(Map.of(
                "game_id", gameId,
                "type_request", "make_move",
                "player", player,
                "move", Map.of("row", row, "col", col)
        ));
        return jsonPayload;
    }

    /**
     * Generates an empty battlefield grid (10x10 initialized with 0s) and places predefined ships.
     * @return A list of maps representing the battlefield grid.
     */
    public static List<Map<String, Integer>> generateBattlefield() {
        int rows = 10;
        int cols = 10;
        List<Map<String, Integer>> battlefield = new ArrayList<>();

        for (int row = 0; row < rows; row++) {
            for (int col = 0; col < cols; col++) {
                battlefield.add(new HashMap<>(Map.of("row", row, "col", col, "value", 0)));
            }
        }

        int[] shipLengths = {3, 4, 5};
        int[][] shipPositions = {
                {0, 0, 1},
                {1, 1, 0},
                {4, 6, 1}
        };

        int shipIndex = 0;
        for (int[] position : shipPositions) {
            int row = position[0];
            int col = position[1];
            int direction = position[2];
            int length = shipLengths[shipIndex];
            shipIndex++;

            for (int i = 0; i < length; i++) {
                if (direction == 1) {
                    setCell(battlefield, row, col + i, 1);
                } else {
                    setCell(battlefield, row + i, col, 1);
                }
            }
        }

        return battlefield;
    }

    /**
     * Sets the value of a specific cell in the battlefield.
     * @param battlefield The battlefield grid.
     * @param row The row index.
     * @param col The column index.
     * @param value The value to set.
     */
    private static void setCell(List<Map<String, Integer>> battlefield, int row, int col, int value) {
        battlefield.stream()
                .filter(cell -> cell.get("row") == row && cell.get("col") == col)
                .forEach(cell -> cell.put("value", value));
    }

    private static final String CHARACTERS = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";

    public static String generateRandomString(int length) {
        Random random = new Random();
        StringBuilder result = new StringBuilder();

        for (int i = 0; i < length; i++) {
            int randomIndex = random.nextInt(CHARACTERS.length()); // Scegli un indice casuale
            result.append(CHARACTERS.charAt(randomIndex)); // Aggiungi il carattere casuale alla stringa
        }

        return result.toString();
    }

    public static void main(String[] args) {
        try {
            String gameID = generateRandomString(20);
            String player1FinalState = "";
            String player2FinalState = "";
            String player1ID = "player1";
            String player2ID = "player2";
            String currentPlayer;
            String requestJson, responseJson;


            // STARTING GAME TEST
            requestJson = createStartGameRequest(gameID, player1ID, player2ID);
            responseJson = sendPostRequest(SERVER_URL , requestJson);
            System.out.println("Server Response: " + responseJson);


            // SIMULATED MATCH TEST
            // Game loop: alternate turns
            Random rand = new Random();
            List<Map<String, Integer>> player1Battlefield = generateBattlefield();
            List<Map<String, Integer>> player2Battlefield = generateBattlefield();

            // List the ship positions for player2 (player1 has to sink them)
            List<Map<String, Integer>> player2ShipPositions = new ArrayList<>();
            for (Map<String, Integer> cell : player2Battlefield) {
                if (cell.get("value") == 1) {
                    player2ShipPositions.add(cell);  // Collect the cells where player2's ships are located
                }
            }

            int turn = 0;
            while (true) {
                currentPlayer = (turn % 2 == 0) ? player1ID : player2ID;
                List<Map<String, Integer>> currentPlayerBattlefield = (turn % 2 == 0) ? player1Battlefield : player2Battlefield;
                List<Map<String, Integer>> opponentBattlefield = (turn % 2 == 0) ? player2Battlefield : player1Battlefield;

                int row = -1, col = -1;

                if (currentPlayer.equals(player1ID) && !player2ShipPositions.isEmpty()) {
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

                requestJson = createMakeMoveRequest(gameID, currentPlayer, row, col);
                responseJson = sendPostRequest(SERVER_URL, requestJson);
                System.out.println("Server Response for " + currentPlayer+": " + responseJson);

                if(responseJson.equals("{\"message\":\"ERROR: Game not found\"}")) {
                    break;  // If the game is not found, exit the loop.
                }

                // Check if the game has ended (e.g., "VICTORY" or "DEFEAT" directly from the response)
                if (responseJson.equals("{\"message\":\"VICTORY\"}")) {
                    if (currentPlayer.equals(player1ID)){
                        player1FinalState = "VICTORY";  // Player1 won
                    }else{
                        player2FinalState = "VICTORY";  // Player2 won
                    }
                }

                if (responseJson.equals("{\"message\":\"DEFEAT\"}")) {
                    if (currentPlayer.equals(player1ID)){
                        player1FinalState = "DEFEAT";  // Player1 lost
                    }else{
                        player2FinalState = "DEFEAT";  // Player2 lost
                    }
                }

                if ((player1FinalState.equals("VICTORY") || player1FinalState.equals("DEFEAT")) &&
                        (player2FinalState.equals("VICTORY") || player2FinalState.equals("DEFEAT"))){
                    break;  // End the game if both players have a final state (either victory or defeat)
                }

                turn++; // Proceed to the next turn
            }

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

}
