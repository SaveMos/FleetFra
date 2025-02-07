let socket;
let player_username = sessionStorage.getItem("userLog");
let game_id;
let player_battlefield;
let timerInterval;
let intervalId = null; // Variabile per memorizzare l'ID del timer
let row, col;
let last_update = null;
let turn;

// Funzione per inizializzare il WebSocket
function initializeWebSocket(current_match, current_battlefield) {

    game_id = current_match;
    player_battlefield = current_battlefield;

    const serverAddress = "ws://10.2.1.30:8080/ws"; // Indirizzo del server Erlang
    socket = new WebSocket(serverAddress);

    socket.addEventListener("open", (event) => {
        console.log("WebSocket connection established:", event);
        //When the websocket is created, the startMessage of the player is sent to the server
        sendStartMessage();
    });

    // Server sent a message
    socket.addEventListener("message", (event) => {
        console.log("Message received from server:", event.data);
        handleServerMessage(event.data);

    });

    // Socket closed
    socket.addEventListener("close", (event) => {
        console.log("WebSocket connection closed:", event);
    });

    // Error during the connection
    socket.addEventListener("error", (event) => {
        console.error("WebSocket error:", event);
    });
}

// Funzione per inviare un messaggio al server WebSocket
function sendStartMessage() {
    if (socket && socket.readyState === WebSocket.OPEN) {

        let message = createBattlefieldJson(player_battlefield);

        setTimeout(function() {
            socket.send(message);
        }, 1000);

        console.log("Message sent:", message);
    } else {
        console.error("WebSocket is not open.");
    }
}
function sendMoveMessage(current_row, current_col) {

    row = current_row;
    col = current_col;

    if (socket && socket.readyState === WebSocket.OPEN) {

        let message = createMoveJson(row, col);

        setTimeout(function() {
            socket.send(message);
        }, 1000);

        console.log("Message sent:", message);
    } else {
        console.error("WebSocket is not open.");
    }
}

function sendGetGameMessage(){

    if (socket && socket.readyState === WebSocket.OPEN) {

        const getGameData = {
            type_request: "get_game_info",
            game_id: game_id
        };

       let message = JSON.stringify(getGameData, null, 2);

        setTimeout(function() {
            socket.send(message);
        }, 1000);

        console.log("Message sent:", message);
    } else {
        console.error("WebSocket is not open.");
    }
}

function sendChangeTurnMessage(){

    if (socket && socket.readyState === WebSocket.OPEN) {

        const getGameData = {
            type_request: "change_turn",
            game_id: game_id
        };

        let message = JSON.stringify(getGameData, null, 2);

        setTimeout(function() {
            socket.send(message);
        }, 1000);

        console.log("Message sent:", message);
    } else {
        console.error("WebSocket is not open.");
    }
}

// Funzione per gestire i messaggi ricevuti dal server
function handleServerMessage(serverMessage) {
    console.log("Handling message:", serverMessage);
    try {
        const message = JSON.parse(serverMessage);
        //Handling the updated information
        if (!message.message) {
            console.log("GET GAME jSON");

            let extractedData = extractGameData(message);

            if(last_update == null){
                last_update = extractedData.created_at;
            }
            // game finished
            if(extractedData.winner !== "none"){
                if(extractedData.winner === player_username){
                    alert("YOU WIN!");
                }else{
                    alert("YOU LOST!");
                }
                // insert match in the database
                reloadPage();
                return;
            }

            if(player_username === extractedData.current_turn){
                turn = true;
                // stop the periodic request if the player has the turn
                stopPeriodicExecution();
                // activate opponent grid
                changeOpponentGrid(true);
                // update grid of the player
                updatePlayerGrid(extractedData.battlefieldMatrix);
                // reset and start the timer
                resetTimer();
                startTimer();


                }else if(player_username === extractedData.waiting_player){
                turn = false;
                // deactivate opponent grid
                changeOpponentGrid(false);
                // periodic requests to the server
                startPeriodicExecution();
                if(last_update !== extractedData.created_at){
                    // update grid of the player
                    updatePlayerGrid(extractedData.battlefieldMatrix);
                    // reset and start the timer
                    resetTimer();
                    startTimer();
                    last_update = extractedData.waiting_player;
                }
            }
            // set the field of the player turn
            setPlayerTurn(extractedData.current_turn);

        }else {

            const responseMessage = message.message;
            // case-insensitive check
            if (/error/i.test(responseMessage) || /invalid/i.test(responseMessage)) {
                console.log("Error: ", responseMessage);
                //MOSTRARE ERRORE E REFRESH PAGINA
                reloadPage();
                return;
            }

            switch (responseMessage) {
                case "OK: Game started":
                    console.log("OK: Game started");
                    sendGetGameMessage();
                    break;
                case "OK: Move accepted [2]":
                    console.log("Move accepted - ship");
                    //Aggiorna solo griglia avversario e chiede info per vedere se ha vinto
                    updateOpponentCell(true);
                    sendGetGameMessage();
                    break;
                case "OK: Move accepted [3]":
                    console.log("Move accepted - water");
                    //Aggiorna solo griglia avversario e richiede aggiornamento per sospendersi
                    updateOpponentCell(false);
                    sendGetGameMessage();
                    break;
                case "OK: Turn changed":
                    console.log("OK: Turn changed");
                    sendGetGameMessage();
                    break;
                case "VICTORY":
                    console.log("Congratulations! You won!");
                    break;
                case "DEFEAT":
                    console.log("Game over. You lost.");
                    break;
                default:
                    console.log("Unknown message");
            }
        }
    } catch (error) {
        console.error("Error parsing JSON message:", error);
    }

}
function updateOpponentCell(sink){
    let boardName = "opponent";

    document.querySelectorAll(".cell").forEach((cell) => {

        // For the cells in the grid1
        if (cell.closest('#grid2')) {

            const [_, row, col] = cell.id.split(',').map(Number);

            // For the cells that contains number of letters the listeners aren't associated
            let currentCell = document.getElementById(`${boardName},${row},${col}`);
            if(sink){
                currentCell.classList.add("sink");
            }else{
                currentCell.classList.add("unavailable");
            }

        }
    });
}
function updatePlayerGrid(grid){
    let boardName = "user";

    document.querySelectorAll(".cell").forEach((cell) => {

        // For the cells in the grid1
        if (cell.closest('#grid1')) {

            const [_, row, col] = cell.id.split(',').map(Number);
            let currentCell = document.getElementById(`${boardName},${row},${col}`);

            if(grid[row][col] === 2) {
                currentCell.classList.add("sink");
            }else if(grid[row][col] === 3){
                currentCell.classList.add("unavailable");
            }
        }
    });
}
function extractGameData(gameData) {


    const { created_at, current_turn, waiting_player, winner, battlefields } = gameData;

    // Estrarre la matrice del giocatore
    let battlefieldMatrix = Array.from({ length: 10 }, () => Array(10).fill(0));
    battlefields[player_username].forEach(({ col, row, value }) => {
        battlefieldMatrix[row][col] = value;
    });

    return {
        created_at,
        current_turn,
        waiting_player,
        winner,
        battlefieldMatrix
    };
}

// Funzione per chiudere il WebSocket
function closeWebSocket() {
    if (socket) {
        socket.close();
        console.log("Closing WebSocket connection.");
    }
}

// Funzione per ricaricare la pagina
function reloadPage() {
    socket.close();
    window.location.reload();
}

function createMoveJson(row, col) {
    const moveData = {
        move: { col: col, row: row },
        type_request: "make_move",
        player: player_username,
        game_id: game_id
    };

    return JSON.stringify(moveData, null, 2);
}

// Funzione per creare un JSON con il campo "player_battlefield" da una matrice
function createBattlefieldJson(matrix) {
    let battlefield = [];

    for (let row = 0; row < matrix.length; row++) {
        for (let col = 0; col < matrix[row].length; col++) {
            battlefield.push({
                row: row,
                col: col,
                value: matrix[row][col]
            });
        }
    }

    const battlefieldData = {
        game_id: game_id,
        player: player_username,
        type_request: "start_game_client",
        player_battlefield: battlefield

    };

    return JSON.stringify(battlefieldData, null, 2);
}


function startTimer() {
    let timeLeftElement = document.getElementById("timeLeft");
    let timeLeft = parseInt(timeLeftElement.textContent);

    if (timerInterval) clearInterval(timerInterval); // Previene doppie esecuzioni

    timerInterval = setInterval(() => {
        if (timeLeft > 0) {
            timeLeft--;
            timeLeftElement.textContent = timeLeft;
        } else {
            clearInterval(timerInterval);
            console.log("Timer scaduto!");
            // only who has the turn, if the timer elapsed, send the request
            if(turn){
                sendChangeTurnMessage();
            }
        }
    }, 1000);
}

function resetTimer() {
    clearInterval(timerInterval);
    document.getElementById("timeLeft").textContent = "10";
}

function setPlayerTurn(player) {
    document.getElementById("playerTurn").textContent = player +" turn";
}

function clearPlayerTurn() {
    document.getElementById("playerTurn").textContent = "";
}


// Funzione per avviare la chiamata periodica di sendGetGameMessage
function startPeriodicExecution() {
    if (intervalId == null) { // Evita di avviare più timer
        intervalId = setInterval(sendGetGameMessage, 1000);
        console.log("Esecuzione periodica avviata");
    }
}

// Funzione per fermare la chiamata periodica
function stopPeriodicExecution() {
    if (intervalId != null) {
        clearInterval(intervalId);
        console.log("Esecuzione periodica fermata");
        intervalId = null;
    }
}

