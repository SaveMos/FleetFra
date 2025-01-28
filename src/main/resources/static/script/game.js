const serverIp = "10.2.1.30";
const serverPort = 8080;
const endpoint = "/game";

const payload = {
    game: "start"
};

let playerGrid
const startGameButton = document.querySelector("#startButton");

//startGameButton.onclick = function() {sendStart()}
startGameButton.onclick = function() {
    sendStart().catch(error => {
        console.error("Error in sendStart:", error);
    });
};


function setUserGrid(grid){
    playerGrid = grid;
}

async function sendStart(){
    const jsonGrid = gridToJson(playerGrid);
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