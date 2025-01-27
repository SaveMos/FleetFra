username = sessionStorage.getItem("userLog");

$(document).ready(function() {
    let request = {
        gameId : sessionStorage.getItem("gameId"),
        role : sessionStorage.getItem("role"),
        usernamePlayer: username
    };

    $.ajax({
        url : "http://10.2.1.120:5050/game",
        data : JSON.stringify(request),
        dataType : "json",
        type : "POST",
        contentType: 'application/json',
        success: function (data) {
            var users = []
            for (var key in data) {
                if((key === "player1" || key === "player2") && data.hasOwnProperty(key)){
                    if(data[key] !== username && sessionStorage.getItem("role") != "Guesser")
                        users[0] = data[key]
                    if(sessionStorage.getItem("role") == "Guesser")
                        users.push(data[key]);
                }
                if(key === "guesser" && data.hasOwnProperty(key) && sessionStorage.getItem("role") != "Guesser")
                    users[1] = data[key]
            }
            sessionStorage.setItem("player1", users[0]);
            sessionStorage.setItem("player2", users[1]);
            location.href = "../game.html";
        },
        error: function(xhr) {
            alert(xhr.responseText);
            location.href = "../playerMainPage.html";
        }
    })
});