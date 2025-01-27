let user_logged = getUsername();
document.addEventListener("DOMContentLoaded", function () {
    checkLogIn();
    setUsername();

    document.getElementById('logout').onclick = function() {
        logout();
    }

    document.getElementById("player").onclick = function(){
        sessionStorage.setItem("role", "Player");
        location.href = "../loading.html";
    }

    document.getElementById("guesser").onclick = function(){
        sessionStorage.setItem("role", "Guesser");
        location.href = "../loading.html";
    }

    document.getElementById("friend_btn").onclick = function(){
        location.href = "../friendsPage.html";
    }

});
function logout() {
    if (!confirm("Are you sure you want to log out?")) return;

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
};
function checkLogIn(){
    if(!sessionStorage.getItem("userLog")){
        alert("User not logged!")
        location.href = "../home.html"
    }
}

function getUsername() {
    return sessionStorage.getItem("userLog") || "Guest";
}

function setUsername() {
    let usernameDisplay = document.getElementById("username-display");
    if (usernameDisplay)
        usernameDisplay.textContent = "Welcome " + getUsername() + "!";
}

$(document).ready(function() {
});

document.getElementById("player").onclick = function(){
    sessionStorage.setItem("role", "Player");
    location.href = "../loading.html";
}

document.getElementById("guesser").onclick = function(){
    sessionStorage.setItem("role", "Guesser");
    location.href = "../loading.html";
}

document.getElementById("friend_btn").onclick = function(){
    location.href = "../friendsPage.html";
}

document.getElementById("bgame_btn").onclick = function(){
    location.href = "../browseGames.html";
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