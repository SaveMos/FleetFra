import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.Scanner;

public class TestErlangServer {

    private static final String SERVER_URL = "http://localhost:8080"; // URL del server Erlang
    private static final String CONTENT_TYPE = "application/json";

    public static void main(String[] args) {
        try {
            // Test: Start a new game
            String startGameJson = """
                {
                    "type_request": "start_game",
                    "player1": "PlayerA",
                    "player2": "PlayerB"
                }
                """;
            sendPostRequest(SERVER_URL, startGameJson);

            // Test: Make a move
            String makeMoveJson = """
                {
                    "type_request": "make_move",
                    "game_id": "PlayerA_PlayerB_1234567890", // Usa un ID reale generato
                    "move_data": {
                        "x": 3,
                        "y": 5
                    }
                }
                """;
            sendPostRequest(SERVER_URL, makeMoveJson);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static void sendPostRequest(String serverUrl, String jsonData) {
        HttpURLConnection connection = null;
        try {
            // Crea la connessione
            URL url = new URL(serverUrl);
            connection = (HttpURLConnection) url.openConnection();
            connection.setRequestMethod("POST");
            connection.setRequestProperty("Content-Type", CONTENT_TYPE);
            connection.setDoOutput(true);

            // Invia i dati JSON
            try (OutputStream os = connection.getOutputStream()) {
                byte[] input = jsonData.getBytes(StandardCharsets.UTF_8);
                os.write(input, 0, input.length);
            }

            // Leggi la risposta dal server
            int responseCode = connection.getResponseCode();
            System.out.println("Response Code: " + responseCode);

            try (Scanner scanner = new Scanner(connection.getInputStream(), StandardCharsets.UTF_8)) {
                String response = scanner.useDelimiter("\\A").next();
                System.out.println("Response Body: " + response);
            }

        } catch (Exception e) {
            System.err.println("Error while sending POST request: " + e.getMessage());
        } finally {
            if (connection != null) {
                connection.disconnect();
            }
        }
    }
}
