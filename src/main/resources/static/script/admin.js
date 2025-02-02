let admin_logged = sessionStorage.getItem("userLog");

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

function searchUsers() {
    const name = document.getElementById("search-name").value.toLowerCase();
    const surname = document.getElementById("search-surname").value.toLowerCase();
    const username = document.getElementById("search-username").value.toLowerCase();

    let requestAdmin = {};
    requestAdmin.firstName = name;
    requestAdmin.lastName = surname;
    requestAdmin.username = username;

    $.ajax({
        url : "http://10.2.1.26:5050/viewDetailedUsers",
        data : JSON.stringify(requestAdmin),
        type : "POST",
        dataType: "json",
        contentType: 'application/json',
        success: function (response) {
            const table = document.getElementById("users-table");

            if (response && Array.isArray(response) && response.length > 0) {
                table.innerHTML = response.map(user => `
                <tr>
                  <td>${user.firstName}</td>
                  <td>${user.lastName}</td>
                  <td>${user.username}</td>
                  <td>${user.email}</td>
                  <td>${user.playedGames}</td>
                  <td>${user.winGames}</td>
                  <td>${user.lostGames}</td>
                </tr>`).join("");

                if ( document.getElementById("emptyUser").innerText.trim() !== "") {
                    document.getElementById("emptyUser").innerText = "";
                }

            }else{
                table.innerHTML = "";
                document.getElementById("emptyUser").innerText = "User not found";
                //alert("User not found");
            }
        },
        error: function(xhr) {
            alert(xhr.responseText);
        }
    })
}

function searchRemovableUsers() {
    const name = document.getElementById("remove-name").value.toLowerCase();
    const surname = document.getElementById("remove-surname").value.toLowerCase();
    const username = document.getElementById("remove-username").value.toLowerCase();

    let requestAdmin = {};
    requestAdmin.firstName = name;
    requestAdmin.lastName = surname;
    requestAdmin.username = username;

    $.ajax({
        url : "http://10.2.1.26:5050/viewUsers",
        data : JSON.stringify(requestAdmin),
        type : "POST",
        dataType: "json",
        contentType: 'application/json',
        success: function (response) {
            const table = document.getElementById("remove-users-table");
            if (response && Array.isArray(response) && response.length > 0) {
                table.innerHTML = response.map(user => `
                <tr id="user-${user.username}">
                  <td>${user.firstName}</td>
                  <td>${user.lastName}</td>
                  <td>${user.username}</td>
                  <td><button class="removeButton" onclick="removeUser('${user.username}')">Remove</button></td>
                </tr>`).join("");

                if ( document.getElementById("removeUser").innerText.trim() !== "") {
                    document.getElementById("removeUser").innerText = "";
                }
            }else{
                table.innerHTML = "";
                document.getElementById("removeUser").innerText = "User not found";
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

function filterMatches() {
    const startDate = new Date(document.getElementById("start-date").value);
    const endDate = new Date(document.getElementById("end-date").value);
    //Dates read are in the format YYYY-MM-DD, the database contains data in the format 2024-12-02T17:00:00Z
    let requestAdmin = {};
    if ((!startDate || isNaN(startDate.getTime()))){
        requestAdmin.date1 = startDate;

    }else{
        requestAdmin.date1 = startDate.toISOString().split("T")[0];
    }
    if ((!endDate || isNaN(endDate.getTime()))) {
        requestAdmin.date2 = endDate;
    }else{
        requestAdmin.date2 = endDate.toISOString().split("T")[0]
    }

    $.ajax({
        url : "http://10.2.1.26:5050/browseGamesAdmin",
        data : JSON.stringify(requestAdmin),
        type : "POST",
        dataType: "json",
        contentType: 'application/json',
        success: function (response) {
            const table = document.getElementById("matches-table");
            if (response && Array.isArray(response) && response.length > 0) {
                table.innerHTML = response.map(match => `
                <tr>
                  <td>${match.user1}</td>
                  <td>${match.user2}</td>
                  <td>${match.timestamp}</td>
                  <td>${match.winner}</td>
                </tr>`).join("");

                if ( document.getElementById("searchMatch").innerText.trim() !== "") {
                    document.getElementById("searchMatch").innerText = "";
                }

            }else{
                table.innerHTML = "";
                document.getElementById("searchMatch").innerText = "Match not found";
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