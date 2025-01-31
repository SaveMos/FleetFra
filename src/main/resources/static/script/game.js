let user_logged = sessionStorage.getItem("userLog");
const serverIp = "10.2.1.30";
const serverPort = 8080;
const endpoint = "/game";

const payload = {
    game: "start"
};

let playerGrid
const startGameButton = document.querySelector("#startButton");
const logoutGameButton = document.querySelector("#homeButton");

// manages the logout of the user
$(document).ready(function () {
    logoutGameButton.onclick = function () {
        $.ajax({
            url: "http://10.2.1.26:5050/logout",
            type: "POST",
            data: user_logged,
            dataType: "text",
            contentType: 'application/json',
            success: function () {
                sessionStorage.removeItem("userLog");
                window.location.href = "index.html";
            },
            error: function (xhr) {
                alert(xhr.responseText);
            }
        });
    };
});

//startGameButton.onclick = function() {sendStart()}
$(document).ready(function () {
    startGameButton.onclick = function () {
        showWaitingScreen();
        sendStart().catch(error => {
            console.error("Error in sendStart:", error);
        });
    };
});

function setUserGrid(grid){
    playerGrid = grid;
}

async function sendStart(){
    const jsonGrid = gridToJson(playerGrid);
    /*
    try {
        const response = await fetch(`http://${serverIp}:${serverPort}${endpoint}`, {
            method: "POST",
            headers: {
                "Content-Type": "application/json"
            },
            body: JSON.stringify(payload)
        });

        if (response.ok) {
            const responseData = await response.json();
            console.log("Server Response:", responseData);
        } else {
            console.error("Error in the server response:", response.status, response.statusText);
        }
    } catch (error) {
        console.error("Error sending the request:", error);
    }
     */
    $.ajax({
        url: "http://10.2.1.26:5050/game",
        data: user_logged,
        type: "POST",
        contentType: 'application/json',

        success: function (response) {
            let jsonResponse = JSON.parse(response);
            console.log("matchID:", jsonResponse.matchId);
            console.log("Player 1:", jsonResponse.player1);
            console.log("Player 2:", jsonResponse.player2);

            sessionStorage.setItem("gameId", jsonResponse.matchId);
            //document.getElementById("playerHeader").innerText = jsonResponse.player1;
            document.getElementById("opponentHeader").innerText = jsonResponse.player2;
            document.getElementById("matchMaking").innerText = "Opponent found!";
            document.getElementById("matchMaking").style.color = "#07C043FF";

            // Dopo 2 secondi, nascondiamo la finestra di attesa
            setTimeout(function() {
                hideWaitingScreen();
            }, 2000);
        },
        error: function (xhr) {
            document.getElementById("matchMaking").innerText = "Opponent not found!";
            document.getElementById("matchMaking").style.color = "#E70448E7";
            // Dopo 2 secondi, nascondiamo la finestra di attesa
            setTimeout(function() {
                hideWaitingScreen();
            }, 2000);
        }
    })

    changeUserGrid()


}
function showWaitingScreen() {
    // Creiamo un overlay per disabilitare l'interazione con la pagina
    let overlay = document.createElement('div');
    overlay.id = "waitingOverlay";
    overlay.style.position = "fixed";
    overlay.style.top = "0";
    overlay.style.left = "0";
    overlay.style.width = "100%";
    overlay.style.height = "100%";
    overlay.style.backgroundColor = "rgba(0, 0, 0, 0.5)";
    overlay.style.display = "flex";
    overlay.style.justifyContent = "center";
    overlay.style.alignItems = "center";
    overlay.style.zIndex = "1000"; // Deve essere sopra gli altri elementi

    // Creiamo il messaggio di attesa
    let message = document.createElement('div');
    message.id = "matchMaking";
    message.style.backgroundColor = "white";
    message.style.padding = "20px";
    message.style.borderRadius = "10px";
    message.style.textAlign = "center";
    message.style.fontSize = "20px";
    message.innerHTML = "Waiting for the opponent...";

    // Aggiungiamo il messaggio all'overlay
    overlay.appendChild(message);

    // Aggiungiamo l'overlay al corpo della pagina
    document.body.appendChild(overlay);

    // Disabilitiamo l'interazione con la pagina
    document.body.style.pointerEvents = "none";
}

// Funzione per nascondere la finestra di attesa
function hideWaitingScreen() {
    // Troviamo e rimuoviamo l'overlay
    let overlay = document.getElementById("waitingOverlay");
    if (overlay) {
        document.body.removeChild(overlay);
    }

    // Riabilitiamo l'interazione con la pagina
    document.body.style.pointerEvents = "auto";
}

// remove the class unavailable to the cells of the grid that are adjacent the ships
// the cells return with the original color
function changeUserGrid(){

    let boardName = "user";

        document.querySelectorAll(".cell").forEach((cell) => {

            // For the cells in the grid1
            if (cell.closest('#grid1')) {

                const [_, row, col] = cell.id.split(',').map(Number);

                // For the cells that contains number of letters the listeners aren't associated
                if(playerGrid[row][col] === -1) {
                    let cell = document.getElementById(`${boardName},${row},${col}`);
                    cell.classList.remove("unavailable");
                }
            }
        });
}

function changeOpponentGrid(){

    let boardName = "opponent";

    document.querySelectorAll(".cell").forEach((cell) => {

        // For the cells in the grid1
        if (cell.closest('#grid2')) {

            const [_, row, col] = cell.id.split(',').map(Number);

            // For the cells that contains number of letters the listeners aren't associated
            if(playerGrid[row][col] === -1) {
                let cell = document.getElementById(`${boardName},${row},${col}`);
                cell.classList.remove("unavailable");
            }
        }
    });
}

function gridToJson(grid) {
    const jsonArray = [];

    for (let row = 0; row < grid.length; row++) {
        for (let col = 0; col < grid[row].length; col++) {
            jsonArray.push({
                row: row,
                col: col,
                value: grid[row][col]
            });
        }
    }

    return JSON.stringify(jsonArray, null, 2);
}