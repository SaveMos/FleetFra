document.addEventListener("DOMContentLoaded", function(){
    checkLogIn();

    generate_games();

    document.getElementById('logout').onclick = function () {
        if(confirm("Are u sure you want to log out?")){
            sessionStorage.removeItem("userLog");
            location.href = "../home.html";
        }
    }

    document.getElementById('home').onclick = function () {
        location.href = "../admin.html";
    }

    document.getElementById('search_btn').onclick = function () {
        search_games();
    }
})

function checkLogIn(){
    if(sessionStorage.getItem("userLog")!=="admin"){
        alert("You Must be logged as Admin to access this web page!")
        location.href = "../home.html";
    }
}

function generate_games() {
    document.getElementById('games_list').innerHTML = "";

    $.ajax({
        url: "http://10.2.1.120:5050/browseGamesAdmin",
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
                    let score = entry.score
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

function search_games() {
    document.getElementById('games_list').innerHTML = "";
    let username = document.getElementById("user_input").value;

    if(username === ""){
        generate_games();
        return;
    }

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
                    row.style.border = '2px solid #006400';
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