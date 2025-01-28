// Sample Data
const users = [
    { name: "John", surname: "Doe", username: "johndoe", email: "john@example.com", gamesPlayed: 10, gamesWon: 6, gamesLost: 4 },
    { name: "Jane", surname: "Smith", username: "janesmith", email: "jane@example.com", gamesPlayed: 12, gamesWon: 8, gamesLost: 4 }
];

const matches = [
    { player1: "johndoe", player2: "janesmith", timestamp: "2024-12-01T15:00:00Z", winner: "johndoe" },
    { player1: "janesmith", player2: "johndoe", timestamp: "2024-12-02T17:00:00Z", winner: "janesmith" }
];

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

// Remove User
function removeUser(username) {
    console.log("Removed user:", username);
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