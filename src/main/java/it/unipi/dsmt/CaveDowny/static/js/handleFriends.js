let user_logged = sessionStorage.getItem("userLog");

document.addEventListener('DOMContentLoaded', function() {
  checkLogIn();

  let usernameValue = sessionStorage.getItem("userLog");
  let usernameParagraph = document.getElementById("username");

  usernameParagraph.style.fontWeight = "bold";
  usernameParagraph.style.fontSize = "1.2em";
  usernameParagraph.style.color = "black";

  usernameParagraph.textContent = usernameValue;

  generate_friends();

  document.getElementById('logout').onclick = function () {
    logout();
  };

  document.getElementById('home').onclick = function (){
    if(sessionStorage.getItem("userLog")){
      location.href = "../playerMainPage.html";
    }
    else{
      alert("You need to login to access the profile page")
      location.href = "../login.html";
    }
  }

  document.getElementById('homeProfile').onclick = function (){
    if(sessionStorage.getItem("userLog")){
      location.href = "../playerMainPage.html";
    }
    else{
      alert("You need to login to access the profile page")
      location.href = "../login.html";
    }
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

function remove_friend(id) {
  return new Promise(function(resolve, reject) {
    let request = {
      username: user_logged,
      usernameFriend: id
    };

    $.ajax({
      url: "http://10.2.1.120:5050/removeFriend",
      data: JSON.stringify(request),
      dataType: "json",
      type: "POST",
      contentType: 'application/json',
      success: function () {
        let element = document.getElementById(id);
        if (element !== null && element.parentNode !== null) {
          element.parentNode.removeChild(element);
          resolve();
        } else {
          reject("Elemento non trovato");
        }
      },
      error: function(xhr) {
        reject(xhr.responseText);
      }
    });
  });
}


function generate_friends(){
  document.getElementById('friend_list').innerHTML = "";
  friends_array = [];
  let request = {
    username : user_logged,
    page : 1
  };

  $.ajax({
    url : "http://10.2.1.120:5050/viewFriends",
    data : JSON.stringify(request),
    dataType : "json",
    type : "POST",
    contentType: 'application/json',
    success: function (data) {
      for(let x = 0; x < data.counter; x++) {
        let friend = data.entries[x];
        let li = document.createElement("li");
        li.id = friend.username1;
        friends_array.push(friend.username1);

        li.style.fontWeight = "bold";
        li.style.fontSize = "1.2em";
        li.style.color = "black";

        li.className = "w3-padding-32";


        var span = document.createElement("span");
        span.className = "w3-large";

        var span0 = document.createElement("span");
        span0.textContent = " " + friend.username1;

        var indicator = document.createElement("span");
        indicator.style.display = "inline-block";
        indicator.style.width = "12px";
        indicator.style.height = "12px";
        indicator.style.borderRadius = "50%";

        if (friend.logged === true) {
          indicator.style.backgroundColor = "green";
        } else {
          indicator.style.backgroundColor = "red";
        }

        var btn = document.createElement('button');
        btn.className="w3-button w3-padding-large w3-white w3-border";
        btn.textContent="Remove";
        btn.style.cssFloat='right';
        btn.addEventListener('click', function() {
          let control = confirm("Are u sure you want to add '"+friend.username1+"' to your friend list?");
          if(!control){
            return;
          }

          remove_friend(friend.username1)
              .then(function() {
                console.log("Friend added.");
              })
              .catch(function(error) {
                console.error("Error:", error);
              })
              .finally(function() {
                location.reload();
              });
        });

        var inviteBtn = document.createElement('button');
        inviteBtn.className="w3-button w3-padding-large w3-white w3-border";
        var b1 = document.createElement("b");
        b1.textContent="Invite";
        inviteBtn.appendChild(b1);
        inviteBtn.style.cssFloat='right';

        inviteBtn.onclick = function(){
          if(document.getElementById("player1Invite").value == "")
            document.getElementById("player1Invite").value = friend.username1;
          else if(document.getElementById("player2Invite").value == "" && document.getElementById("player1Invite").value != friend.username1)
            document.getElementById("player2Invite").value = friend.username1;
        }

        li.appendChild(span);
        li.appendChild(indicator);
        li.appendChild(span0);
        li.appendChild(btn);
        li.appendChild(inviteBtn);
        document.getElementById('friend_list').appendChild(li);
      }
    },
    error: function(xhr) {
      alert(xhr.responseText)
    }
  })
}

function searchGlobalFriends(){
  let friend_to_search = document.getElementById("friend_input").value
  document.getElementById("friend_input").value = "";
  let data = {
    usernameToSearch : friend_to_search
  };

  $.ajax({
    url : "http://10.2.1.120:5050/globalSearch",
    data : JSON.stringify(data),
    dataType : "json",
    type : "POST",
    contentType: 'application/json',
    success: function (response) {
      if (response !== null && Object.keys(response).length > 0) {
        let friendDetails = document.getElementById('friendDetails');
        let rectangleDiv = document.createElement('div');
        rectangleDiv.classList.add('w3-card', 'w3-margin');
        rectangleDiv.style.padding = '10px';
        rectangleDiv.style.border = '2px solid #006400';

        let usernameParagraph = document.createElement('p');
        usernameParagraph.innerHTML = '<strong style="font-size: larger;">Username: '+ response.username +'</strong> ';


        let addButton = document.createElement('button');
        addButton.textContent = 'Add Friend';
        addButton.className = "w3-button w3-teal";
        addButton.addEventListener('click', function() {
          let control = confirm("Are u sure you want to add '"+response.username+"' to your friends?");
          if(!control){
            return;
          }

          addFriend(response.username)
              .then(function() {
                location.reload();
              })
              .catch(function(error) {
                alert("Error: " + error);
              });
        });

        if(response.username === user_logged || already_friends(response.username)){
          addButton.disabled = true;

          addButton.style.backgroundColor = "#ccc";
          addButton.style.cursor = "not-allowed";
        }

        rectangleDiv.appendChild(usernameParagraph);
        rectangleDiv.appendChild(addButton);

        friendDetails.innerHTML = '';
        friendDetails.insertBefore(rectangleDiv, friendDetails.firstChild);
      } else {
        alert("User not found");
      }
    },
    error: function(xhr) {
      alert(xhr.responseText)
    }
  })
}


function addFriend(username) {
  return new Promise(function(resolve, reject) {
    let request = {
      username: user_logged,
      usernameFriend: username
    };

    $.ajax({
      url: "http://10.2.1.120:5050/addFriend",
      data: JSON.stringify(request),
      dataType: "json",
      type: "POST",
      contentType: 'application/json',
      success: function () {
        resolve();
      },
      error: function(xhr) {
        reject(xhr);
      },
      complete: function() {
        location.reload();
      }
    });
  });
}

function already_friends(friend_to_search){
  return friends_array.includes(friend_to_search);
}

document.getElementById("sendInvites").onclick = function(){
  if(document.getElementById("guesser1").checked && document.getElementById("guesser2").checked){
    alert("Invalid invitation: two guessers canno't play in the same match");
    return;
  }

  let request = {
    gameId: sessionStorage.getItem("userLog"),
    player1: document.getElementById("player1Invite").value,
    player2: document.getElementById("player2Invite").value,
    userInvite: sessionStorage.getItem("userLog"),
    role1: document.querySelector('input[name="p1"]:checked').value,
    role2: document.querySelector('input[name="p2"]:checked').value
  };

  $.ajax({
    url: "http://10.2.1.120:5050/inviteFriend",
    data: JSON.stringify(request),
    dataType: "text",
    type: "POST",
    contentType: 'application/json',
    success: function () {
      sessionStorage.setItem("gameId", sessionStorage.getItem("userLog"));
      if(document.getElementById("guesser1").checked || document.getElementById("guesser2").checked)
        sessionStorage.setItem("role", "Player");
      else
        sessionStorage.setItem("role", "Guesser");
      location.href = "../loading.html";
    },
    error: function(xhr) {
     alert("Error sending invites");
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