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

public class FleetFraTest {

    private static final String SERVER_URL = "http://10.2.1.30:8080";

    // Utility per inviare una richiesta POST con un payload JSON
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

    // Funzione per creare la richiesta di avvio del gioco
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

    // Funzione per creare la richiesta di fare una mossa
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

    // Funzione per generare una griglia di battaglia vuota (ad esempio 10x10 con 0)
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

    // Funzione per impostare il valore di una cella nel campo di battaglia
    private static void setCell(List<Map<String, Integer>> battlefield, int row, int col, int value) {
        battlefield.stream()
                .filter(cell -> cell.get("row") == row && cell.get("col") == col)
                .forEach(cell -> cell.put("value", value));
    }

    // Funzione per ottenere il valore di una cella nel campo di battaglia
    private static int getCell(List<Map<String, Integer>> battlefield, int row, int col) {
        return battlefield.stream()
                .filter(cell -> cell.get("row") == row && cell.get("col") == col)
                .map(cell -> cell.get("value"))
                .findFirst()
                .orElse(0);
    }

    // Funzione per verificare se un giocatore ha vinto (tutte le sue navi sono affondate)
    public static boolean checkForVictory(List<Map<String, Integer>> battlefield) {
        return battlefield.stream().allMatch(cell -> cell.get("value") == 0); // Verifica che tutte le celle delle navi siano colpite
    }

    public static void main(String[] args) {
        try {
            String gameID = "game123";
            String player1FinalState = "";
            String player2FinalState = "";
            String player1ID = "player1";
            String player2ID = "player2";

            // Invia la richiesta di avvio del gioco
            String startGameJson = createStartGameRequest(gameID, player1ID, player2ID);
            String startGameResponse = sendPostRequest(SERVER_URL , startGameJson);
            System.out.println("Start Game Response: " + startGameResponse);

            // Eseguiamo il ciclo di gioco alternando le mosse
            Random rand = new Random();
            List<Map<String, Integer>> player1Battlefield = generateBattlefield();
            List<Map<String, Integer>> player2Battlefield = generateBattlefield();

            // Lista delle posizioni delle navi per player1 (che deve affondarle tutte)
            List<Map<String, Integer>> player2ShipPositions = new ArrayList<>();
            for (Map<String, Integer> cell : player2Battlefield) {
                if (cell.get("value") == 1) {
                    player2ShipPositions.add(cell);
                }
            }

            int turn = 0;
            while (true) {
                String currentPlayer = (turn % 2 == 0) ? player1ID : player2ID;
                List<Map<String, Integer>> currentPlayerBattlefield = (turn % 2 == 0) ? player1Battlefield : player2Battlefield;
                List<Map<String, Integer>> opponentBattlefield = (turn % 2 == 0) ? player2Battlefield : player1Battlefield;

                int row = -1, col = -1;

                if (currentPlayer.equals(player1ID) && player2ShipPositions.size() > 0) {
                    // Player1 colpisce le posizioni delle navi di player2 senza sbagliare
                    Map<String, Integer> targetCell = player2ShipPositions.get(0);
                    row = targetCell.get("row");
                    col = targetCell.get("col");

                    // Rimuovi la nave colpita dalla lista
                    player2ShipPositions.remove(0);
                } else {
                    // Player2 spara a caso
                    row = rand.nextInt(10);
                    col = rand.nextInt(10);
                }

                String makeMoveJson = createMakeMoveRequest(gameID, currentPlayer, row, col);
                String makeMoveResponse = sendPostRequest(SERVER_URL, makeMoveJson);
                System.out.println(currentPlayer + " makes move to (" + row + ", " + col + "): " + makeMoveResponse);

                // Stampa direttamente il contenuto della risposta senza fare parsing
                System.out.println("Erlang response: " + makeMoveResponse);

                if(makeMoveResponse.equals("{\"message\":\"Game not found\"}")) {
                    break;
                }

                // Verifica se il gioco è terminato (ad esempio, controllo di "VICTORY" o "DEFEAT" direttamente dalla risposta)
                if (makeMoveResponse.equals("{\"message\":\"VICTORY\"}")) {
                    if (currentPlayer.equals(player1ID)){
                        player1FinalState = "VICTORY";
                    }else{
                        player2FinalState = "VICTORY";
                    }
                }

                if (makeMoveResponse.equals("{\"message\":\"DEFEAT\"}")) {
                    if (currentPlayer.equals(player1ID)){
                        player1FinalState = "DEFEAT";
                    }else{
                        player2FinalState = "DEFEAT";
                    }
                }

                if ((player1FinalState == "VICTORY" || player1FinalState == "DEFEAT") &&
                        (player2FinalState == "VICTORY" || player2FinalState == "DEFEAT")){
                    break;
                }

                turn++; // Passa al turno successivo
            }

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

}
