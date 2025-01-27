var socket;
var points = 0;
var timerInterval;

$(document).ready(function() {
    socket = new WebSocket("ws://10.2.1.86/erlServer");
    document.getElementById("score").textContent = points;
    document.getElementById("send-button").disabled = true;
    document.getElementById("guess-button").disabled = true;
    document.getElementById("restart-button").disabled = true;
    if(sessionStorage.getItem("role") == "Guesser"){
        document.getElementById("chat-box").removeChild(document.getElementById('column1'));
    }
        
    // Socket opened
    socket.addEventListener("open", (event) => {
        console.log("Connessione WebSocket aperta:", event);
        sendStartMsg();
    });

    // Server sent a message
    socket.addEventListener("message", (event) => {
        console.log("Dati ricevuti dal server:", event.data);

        // formato messaggio type, msg
        var messaggioJSON = JSON.parse(event.data);
        console.log("Messaggio decodificato:", messaggioJSON);

        // Ora puoi accedere alle proprietà del tuo oggetto JSON
        var type = messaggioJSON.type;
        var msg = messaggioJSON.msg;

        // Fai qualcosa in base al tipo di messaggio ricevuto
        switch (type) {
            // Ricezione parola iniziale
            case "wordToGuess":
                document.getElementById("restart-button").disabled = true;
                timerInterval = setInterval(updateTimer, 1000);
                printWordToGuess(msg);
                if(sessionStorage.getItem("role") === "Guesser") {
                    document.getElementById("guess-button").disabled = false;
                    document.getElementById("turn-indicator").textContent = "GAME STARTED!";
                    startWait();
                }
                else {
                    if (sessionStorage.getItem("player1") > sessionStorage.getItem("userLog")) {
                        document.getElementById("turn-indicator").textContent = "WAITING FOR A WORD";
                        startWait();
                    } else {
                        document.getElementById("send-button").disabled = false;
                        document.getElementById("turn-indicator").textContent = "IT'S YOUR TURN";
                    }
                }
                break;
            // Ricezione parola durante gioco
            case "word":
                printFriendWord(msg);
                if(sessionStorage.getItem("role") === "Guesser") {
                    console.log("qui")
                    //startWait();
                }
                if(sessionStorage.getItem("role") === "Player") {
                    var guessedWordElement = document.getElementById("guessedWord");
                    while (guessedWordElement.firstChild) {
                        guessedWordElement.removeChild(guessedWordElement.firstChild);
                    }
                    document.getElementById("send-button").disabled = false;
                    document.getElementById("turn-indicator").textContent = "IT'S YOUR TURN";
                }
                break;
            // Ricezione tentativo indovinare
            case "guessWord":
                printGuessedWord(msg);
                var r = messaggioJSON.res;
                if(r === "ok") {
                    points += 1;
                    document.getElementById("score").textContent = points;
                    console.log("OK")
                }
                if(r === "no") {
                    if(points > 0)
                        points -= 1;
                    document.getElementById("score").textContent = points;
                    console.log("NO")
                }
                var w = messaggioJSON.newWord;
                printWordToGuess(w);
                break;
            // Ricezione risultato tentativo indovinare
            case "guessResult":
                if(msg === "ok") {
                    points += 1;
                    document.getElementById("score").textContent = points;
                }
                if(msg === "no") {
                    if(points > 0)
                        points -= 1;
                    document.getElementById("score").textContent = points;
                }

        }
    });

    // Socket closed
    socket.addEventListener("close", (event) => {
        console.log("Connessione WebSocket chiusa:", event);
    });

    // Error during the connection
    socket.addEventListener("error", (event) => {
        console.error("Errore WebSocket:", event);
    });

    if(sessionStorage.getItem("role") === "Player") {
        document.getElementById("guess-button").disabled = true;
    }
    else {
        document.getElementById("send-button").disabled = true;
    }

});

function sendStartMsg(){
    if(sessionStorage.getItem("role") === "Guesser"){
        let messLog = {
            action : "login",
            username : sessionStorage.getItem("userLog")
        };

        let mess = {
            action : "start",
            otherPlayer1 : sessionStorage.getItem("player1"),
            otherPlayer2: sessionStorage.getItem("player2"),
            role: sessionStorage.getItem("role")
        };
    
        setTimeout(function() {
            socket.send(JSON.stringify(messLog));
            socket.send(JSON.stringify(mess));
        }, 1000);
    }
    else{
        let messLog = {
            action : "login",
            username : sessionStorage.getItem("userLog")
        };

        let mess = {
            action : "start",
            otherPlayer1 : sessionStorage.getItem("player1"),
            otherPlayer2: sessionStorage.getItem("player2"),
            role: sessionStorage.getItem("role")
        };
    
        socket.send(JSON.stringify(messLog));
        socket.send(JSON.stringify(mess));
        console.log(messLog);
        console.log(mess);
    }
}

var stringaRisultante;
function printWordToGuess(msg) {
    // Rimuovi tutte le parole precedenti
    var wordToGuessElement = document.getElementById("wordToGuess");
    while (wordToGuessElement.firstChild) {
        wordToGuessElement.removeChild(wordToGuessElement.firstChild);
    }

    // Aggiungi la nuova parola
    stringaRisultante = String.fromCharCode.apply(null, msg);
    var word = document.createTextNode("Word to guess: "+stringaRisultante);
    wordToGuessElement.appendChild(word);
}


function printGuessedWord(msg) {
    // Rimuovi tutte le parole precedenti
    var guessedWordElement = document.getElementById("guessedWord");
    while (guessedWordElement.firstChild) {
        guessedWordElement.removeChild(guessedWordElement.firstChild);
    }

    document.getElementById("column1").innerHTML="";
    document.getElementById("column2").innerHTML="";

    // Aggiungi la nuova parola
    var word = document.createTextNode("Guesser said: "+msg);
    guessedWordElement.appendChild(word);
}


function startWait(){
    
    let mess = {
        action : "wait"
    };
    if (socket.readyState === WebSocket.OPEN) {
        socket.send(JSON.stringify(mess));
    } else {
        console.error("Connessione WebSocket non aperta.");
    }
}

function sendMessage(word){
    var guessedWordElement = document.getElementById("guessedWord");
    while (guessedWordElement.firstChild) {
        guessedWordElement.removeChild(guessedWordElement.firstChild);
    }
    let mess = {
        action : "word",
        word : word
    };
    if (socket.readyState === WebSocket.OPEN) {
        socket.send(JSON.stringify(mess));
    } else {
        console.error("Connessione WebSocket non aperta.");
    }
    document.getElementById("turn-indicator").textContent = "WAITING FOR A WORD";
    document.getElementById("send-button").disabled = true;
    startWait();
}

function guess(){
    var word = document.getElementById("message-input").value;
    let mess = {
        action : "guess",
        word : word
    };
    if (socket.readyState === WebSocket.OPEN) {
        socket.send(JSON.stringify(mess));
    } else {
        console.error("Connessione WebSocket non aperta.");
    }
    document.getElementById("column2").innerHTML="";
    startWait();
}

function printFriendWord(msg){
    var word = document.createTextNode(msg);
    var lineBreak = document.createElement("br");
    document.getElementById("column2").appendChild(word);
    document.getElementById("column2").appendChild(lineBreak);
    document.getElementById("column1").appendChild(document.createElement("br"));
}

function restart(){
    socket.close();
    window.location.reload();
}

document.getElementById("send-button").onclick = function(){
    var w = document.getElementById("message-input").value;
    
    console.log(w);
    console.log(stringaRisultante);
    //se la parola è la stessa di quella da indovinare non viene inviata
    if(w == stringaRisultante){
        document.getElementById("message-input").value = "";
        return;
    }

    //se parola contiene spazi non viene inviata
    if(w.indexOf(' ') !== -1){
        document.getElementById("message-input").value = "";
        return;
    }
   
    var word = document.createTextNode(document.getElementById("message-input").value);
    var lineBreak = document.createElement("br");
    document.getElementById("column1").appendChild(word);
    document.getElementById("column1").appendChild(lineBreak);
    document.getElementById("column2").appendChild(document.createElement("br"));
    document.getElementById("message-input").value = "";
    sendMessage(w);
}

document.getElementById("guess-button").onclick = function(){
    guess();
    document.getElementById("message-input").value = "";
}

var seconds = 60;

function updateTimer(){
    seconds -= 1;
    document.getElementById("timer").textContent = seconds;
    if(seconds <= 0) {
        clearInterval(timerInterval);
        if(sessionStorage.getItem("role") === "Guesser"){
            insert_match()
        }
        document.getElementById("restart-button").disabled = false;
        document.getElementById("send-button").disabled = true;
        document.getElementById("guess-button").disabled = true;
    }
}

function insert_match(){
    let timestamp =  new Date();

    let user1 = sessionStorage.getItem("userLog")
    let user2 = sessionStorage.getItem("player1")
    let user3 = sessionStorage.getItem("player2")

    let match = {
        user1: user1,
        user2: user2,
        user3: user3,
        score: points,
        timestamp: timestamp
    };

    $.ajax({
        url : "http://10.2.1.120:5050/insertMatch",
        data : JSON.stringify(match),
        type : "POST",
        dataType: "text",
        contentType: 'application/json',
        success: function (data) {
            console.log(data)
        },
        error: function(xhr) {
            console.log(xhr)
            alert(xhr)
        }
    })
}


