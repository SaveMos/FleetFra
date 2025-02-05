let socket;
let player_username = sessionStorage.getItem("userLog");
let game_id;
let player_battlefield;
let timerInterval;

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
function sendMoveMessage(row, col) {

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

// Funzione per gestire i messaggi ricevuti dal server
function handleServerMessage(serverMessage) {
    console.log("Handling message:", serverMessage);
    try {
        const message = JSON.parse(serverMessage);

        if (!message.message) {
            console.log("GET GAME jSON");


        }else {

            const responseMessage = message.message;
            // case-insensitive check
            if (/error/i.test(responseMessage) || /invalid/i.test(responseMessage)) {
                console.log("Error: ", responseMessage);
                //MOSTRARE ERRORE E REFRESH PAGINA
                return;
            }

            switch (responseMessage) {
                case "OK: Game started":
                    console.log("OK: Game started");
                    sendGetGameMessage();
                    break;
                case "OK: Move accepted":
                    console.log("Move accepted");
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
        }
    }, 1000);
}

function resetTimer() {
    clearInterval(timerInterval);
    document.getElementById("timeLeft").textContent = "10";
}

function setPlayerTurn() {
    document.getElementById("playerTurn").textContent = "Player Turn";
}

function clearPlayerTurn() {
    document.getElementById("playerTurn").textContent = "";
}

