let user_logged = sessionStorage.getItem("userLog")
document.addEventListener("DOMContentLoaded", function () {
    checkLogIn();

    let usernameValue = sessionStorage.getItem("userLog");
  let usernameParagraph = document.getElementById("username");

  usernameParagraph.style.fontWeight = "bold";
  usernameParagraph.style.fontSize = "1.2em";
  usernameParagraph.style.color = "black";

  usernameParagraph.textContent = usernameValue;
  
    generate_games();

    document.getElementById('logout').onclick = function () {
        logout();
    };

    document.getElementById('home').onclick = function () {
        location.href = "../playerMainPage.html";
    }
});

function logout(){
    if(!confirm("Are you sure you want to log out?")) return;

    $.ajax({
        url: "http://10.2.1.120:5050/logout",
        type: "POST",
        data: user_logged,
        dataType: "text",
        contentType: 'application/json',
        success: function () {
            sessionStorage.removeItem("userLog");
            location.href = "../login.html";
        },
        error: function (xhr) {
            alert(xhr.responseText);
        }
    });
}
function checkLogIn(){
    if(!sessionStorage.getItem("userLog")){
        alert("User not logged!")
        location.href = "../home.html"
    }
}

function generate_games() {
    document.getElementById('games_list').innerHTML = "";
    let username = sessionStorage.getItem("userLog")

    $.ajax({
        url: "http://10.2.1.120:5050/browseGames",
        data: username,
        type: "POST",
        contentType: 'application/json',
        success: function(data) {
            var jsonData = JSON.parse(data);

            var entries = jsonData.entries;

            if (entries) {
                entries.forEach(function (entry) {
                    let user1 = entry.user1;
                    let user2 = entry.user2;
                    let user3 = entry.user3;
                    let score = entry.score;
                    let timestamp = new Date(entry.timestamp).toLocaleString();

                    let row = document.createElement('div');
                    row.classList.add('game-entry');
                    row.innerHTML = `
                        <p><strong>User1:</strong> ${user1}</p>
                        <p><strong>User2:</strong> ${user2}</p>
                        <p><strong>User3:</strong> ${user3}</p>
                        <p><strong>Score:</strong> ${score}</p>
                        <p><strong>Timestamp:</strong> ${timestamp}</p>
                    `;

                    document.getElementById('games_list').appendChild(row);
                });
            } else {
                console.error('La proprietà "entries" è mancante o non è definita nella risposta JSON.');
            }
        },
        error: function (xhr) {
            alert(xhr.responseText);
        }
    });
}

var searchInvite = setInterval(searchInvitation, 10000);

function searchInvitation(){
    $.ajax({
        url : "http://10.2.1.120:5050/checkInvite",
        data : sessionStorage.getItem("userLog"),
        dataType : "json",
        type : "POST",
        contentType: 'application/json',
        success: function (data, textStatus, jqXHR) {
            if(jqXHR.status !== 204){
                let result = window.confirm("Invite received from " + data.userInvite + ", accept?");
                sessionStorage.setItem("gameId", data.id);
                if(result){
                    sessionStorage.setItem("role", data.role);
                    location.href = "../loading.html";
                }
                else{
                    declineInvitation();
                }
            }

        },
        error: function(xhr) {
        }
    })
}

function declineInvitation(){
    $.ajax({
        url : "http://10.2.1.120:5050/declineInvitation",
        data : sessionStorage.getItem("gameId"),
        dataType : "json",
        type : "POST",
        contentType: 'application/json',
        success: function (data) {
        },
        error: function(xhr) {
        }
    })
}