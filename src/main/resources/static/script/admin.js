let admin_logged = sessionStorage.getItem("userLog");
// Sample Data
const users = [
    { name: "John", surname: "Doe", username: "johndoe", email: "john@example.com", gamesPlayed: 10, gamesWon: 6, gamesLost: 4 },
    { name: "Jane", surname: "Smith", username: "janesmith", email: "jane@example.com", gamesPlayed: 12, gamesWon: 8, gamesLost: 4 }
];

const matches = [
    { player1: "johndoe", player2: "janesmith", timestamp: "2024-12-01T15:00:00Z", winner: "johndoe" },
    { player1: "janesmith", player2: "johndoe", timestamp: "2024-12-02T17:00:00Z", winner: "janesmith" }
];

const logoutGameButton = document.querySelector("#logoutButton");

// manages the logout of the admin
$(document).ready(function () {
    logoutGameButton.onclick = function () {
        $.ajax({
            url: "http://10.2.1.26:5050/logout",
            type: "POST",
            data: admin_logged,
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

// Show and hide pages
function showPage(pageId) {
    document.querySelectorAll(".page").forEach(page => {
        page.style.display = page.id === pageId ? "block" : "none";
    });
}

// Search Users
function searchUsers() {
    const name = document.getElementById("search-name").value.toLowerCase();
    const surname = document.getElementById("search-surname").value.toLowerCase();
    const username = document.getElementById("search-username").value.toLowerCase();
    const results = users.filter(user =>
        user.name.toLowerCase().includes(name) &&
        user.surname.toLowerCase().includes(surname) &&
        user.username.toLowerCase().includes(username)
    );
    const table = document.getElementById("users-table");
    table.innerHTML = results.map(user => `
        <tr>
          <td>${user.name}</td>
          <td>${user.surname}</td>
          <td>${user.username}</td>
          <td>${user.email}</td>
          <td>${user.gamesPlayed}</td>
          <td>${user.gamesWon}</td>
          <td>${user.gamesLost}</td>
        </tr>`).join("");
}

function searchUsers1() {
    const name = document.getElementById("search-name").value.toLowerCase();
    const surname = document.getElementById("search-surname").value.toLowerCase();
    const username = document.getElementById("search-username").value.toLowerCase();

    let requestAdmin = {};
    if (name) requestAdmin.firstName = name;
    if (surname) requestAdmin.lastName = surname;
    if (username) requestAdmin.username = username;

    $.ajax({
        url : "http://10.2.1.26:5050/viewDetailedUsers",
        data : JSON.stringify(requestAdmin),
        type : "POST",
        dataType: "json",
        contentType: 'application/json',
        success: function (response) {
            if (response && Array.isArray(response.content) && response.content.length > 0) {
                const table = document.getElementById("users-table");
                table.innerHTML = response.content.map(user => `
                <tr>
                  <td>${user.name}</td>
                  <td>${user.surname}</td>
                  <td>${user.username}</td>
                  <td>${user.email}</td>
                  <td>${user.gamesPlayed}</td>
                  <td>${user.gamesWon}</td>
                  <td>${user.gamesLost}</td>
                </tr>`).join("");
            }else{
                alert("User not found");
            }
        },
        error: function(xhr) {
            alert(xhr.responseText);
        }
    })
}

// Search Removable Users
function searchRemovableUsers() {
    const name = document.getElementById("remove-name").value.toLowerCase();
    const surname = document.getElementById("remove-surname").value.toLowerCase();
    const username = document.getElementById("remove-username").value.toLowerCase();
    const results = users.filter(user =>
        user.name.toLowerCase().includes(name) &&
        user.surname.toLowerCase().includes(surname) &&
        user.username.toLowerCase().includes(username)
    );
    const table = document.getElementById("remove-users-table");
    table.innerHTML = results.map(user => `
        <tr>
          <td>${user.name}</td>
          <td>${user.surname}</td>
          <td>${user.username}</td>
          <td><button class="removeButton" onclick="removeUser('${user.username}')">Remove</button></td>
        </tr>`).join("");
}

function searchRemovableUsers1() {
    const name = document.getElementById("search-name").value.toLowerCase();
    const surname = document.getElementById("search-surname").value.toLowerCase();
    const username = document.getElementById("search-username").value.toLowerCase();

    let requestAdmin = {};
    if (name) requestAdmin.firstName = name;
    if (surname) requestAdmin.lastName = surname;
    if (username) requestAdmin.username = username;

    $.ajax({
        url : "http://10.2.1.26:5050/viewUsers",
        data : JSON.stringify(requestAdmin),
        type : "POST",
        dataType: "json",
        contentType: 'application/json',
        success: function (response) {
            if (response && Array.isArray(response.content) && response.content.length > 0) {
                const table = document.getElementById("users-table");
                table.innerHTML = response.content.map(user => `
                <tr id="user-${user.username}">
                  <td>${user.name}</td>
                  <td>${user.surname}</td>
                  <td>${user.username}</td>
                  <td><button class="removeButton" onclick="removeUser('${user.username}')">Remove</button></td>
                </tr>`).join("");
            }else{
                alert("User not found");
            }
        },
        error: function(xhr) {
            alert(xhr.responseText);
        }
    })
}

// Remove User
function removeUser(username) {

    $.ajax({
        url: "http://10.2.1.26:5050/removeUser",
        data: username,
        dataType : "Text",
        type: "POST",
        contentType: 'application/json',
        success: function () {
            const row = document.getElementById(`user-${username}`);
            if (row) {
                row.remove();
                console.log("User removed from table:", username);
            }
        },
        error: function(xhr) {
            alert(xhr.responseText);
        }
    });
}

// Filter Matches
function filterMatches() {
    const startDate = new Date(document.getElementById("start-date").value);
    const endDate = new Date(document.getElementById("end-date").value);
    const results = matches.filter(match => {
        const matchDate = new Date(match.timestamp);
        return matchDate >= startDate && matchDate <= endDate;
    });
    const table = document.getElementById("matches-table");
    table.innerHTML = results.map(match => `
        <tr>
          <td>${match.player1}</td>
          <td>${match.player2}</td>
          <td>${match.timestamp}</td>
          <td>${match.winner}</td>
        </tr>`).join("");
}

function filterMatches1() {
    const startDate = new Date(document.getElementById("start-date").value);
    const endDate = new Date(document.getElementById("end-date").value);
    //Dates read are in the format YYYY-MM-DD, the database contains data in the format 2024-12-02T17:00:00Z
    let requestAdmin = {};
    if (startDate) requestAdmin.start = startDate;
    if (endDate) requestAdmin.end = endDate;

    $.ajax({
        url : "http://10.2.1.26:5050/browseGamesAdmin",
        data : JSON.stringify(requestAdmin),
        type : "POST",
        dataType: "json",
        contentType: 'application/json',
        success: function (response) {
            if (response && Array.isArray(response.content) && response.content.length > 0) {
                const table = document.getElementById("matches-table");
                table.innerHTML = response.content.map(match => `
                <tr>
                  <td>${match.player1}</td>
                  <td>${match.player2}</td>
                  <td>${match.timestamp}</td>
                  <td>${match.winner}</td>
                </tr>`).join("");

            }else{
                alert("Match not found");
            }
        },
        error: function(xhr) {
            alert(xhr.responseText);
        }
    })

}

document.addEventListener('DOMContentLoaded', () => {
    const inputs = document.querySelectorAll('input');
    inputs.forEach(input => {
        input.addEventListener('focus', () => {
            input.placeholder = '';
        });
        input.addEventListener('blur', () => {
            if (input.id === 'search-name') input.placeholder = 'Name';
            if (input.id === 'search-surname') input.placeholder = 'Surname';
            if (input.id === 'search-username') input.placeholder = 'Username';
            if (input.id === 'remove-name') input.placeholder = 'Name';
            if (input.id === 'remove-surname') input.placeholder = 'Surname';
            if (input.id === 'remove-username') input.placeholder = 'Username';
        });
    });
});