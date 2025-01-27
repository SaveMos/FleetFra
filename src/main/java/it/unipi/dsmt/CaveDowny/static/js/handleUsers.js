document.addEventListener('DOMContentLoaded',function () {
  checkLogIn();
  generate_users();

  document.getElementById('logout').onclick = function () {
    if(confirm("Are u sure you want to log out?"))
      location.href = "../home.html";
  }

  document.getElementById('home').onclick = function () {
    if(sessionStorage.getItem("userLog")){
      location.href = "../admin.html";
    }
    else{
      alert("You need to login to access the profile page")
      location.href = "../login.html";
    }
  }

  document.getElementById('search_btn').onclick = function () {
    let user_to_search = document.getElementById("user_input").value
    document.getElementById("user_input").value = "";

    let data = {
      usernameToSearch : user_to_search
    };

    $.ajax({
      url : "http://10.2.1.120:5050/globalSearch",
      data : JSON.stringify(data),
      dataType : "json",
      type : "POST",
      contentType: 'application/json',
      success: function (response) {
        if (response !== null && Object.keys(response).length > 0) {
          let userDetails = document.getElementById('userDetails');
          let rectangleDiv = document.createElement('div');
          rectangleDiv.classList.add('w3-card', 'w3-margin');
          rectangleDiv.style.padding = '10px';
          rectangleDiv.style.border = '2px solid #006400';

          let usernameParagraph = document.createElement('p');
          usernameParagraph.innerHTML = '<strong style="font-size: larger;">Username: '+ response.username +'</strong> ';


          let addButton = document.createElement('button');
          addButton.textContent = 'Remove';
          addButton.className = "w3-button w3-teal";
          addButton.addEventListener('click', function() {
            let control = confirm("Are u sure you want to delete '"+response.username+"' from your database?");
            if(!control){
              return;
            }

            remove_user(response.username)
                .then(function() {
                  location.reload();
                })
                .catch(function(error) {
                  alert("Error: " + error);
                });
          });


          rectangleDiv.appendChild(usernameParagraph);
          rectangleDiv.appendChild(addButton);

          userDetails.innerHTML = '';
          userDetails.insertBefore(rectangleDiv, userDetails.firstChild);
        } else {
          alert("User not found");
        }
      },
      error: function(xhr) {
        alert(xhr.responseText)
      }
    })
  }
});

function generate_users() {
  document.getElementById('users_list').innerHTML = "";
  let request = {
    page: 1
  };

  $.ajax({
    url: "http://10.2.1.120:5050/viewUsers",
    data: JSON.stringify(request),
    dataType: "json",
    type: "POST",
    contentType: 'application/json',
    success: function (data) {
      for (let x = 0; x < data.counter; x++) {
        let user = data.entries[x];
        let li = document.createElement("li");
        li.id = user.username;

        li.style.fontWeight = "bold";
        li.style.fontSize = "1.2em";
        li.style.color = "black";

        li.className = "w3-padding-32";

        let span = document.createElement("span");
        span.className = "w3-large";
        let span0 = document.createElement("span");
        span0.textContent = user.username;

        let btn = document.createElement('button');
        btn.className = "w3-button w3-padding-large w3-white w3-border";
        btn.textContent = "REMOVE";
        btn.style.cssFloat = 'right';
        btn.addEventListener('click', function() {
          let control = confirm("Are u sure you want to delete the user '"+user.username+"' from the database?");
          if(!control){
            return;
          }

          remove_user(user.username)
              .then(function() {
                console.log("User removed.");
              })
              .catch(function(error) {
                console.error("Error:", error);
              })
              .finally(function() {
                location.reload();
              });
        });

        li.appendChild(span);
        li.appendChild(span0);
        li.appendChild(btn);
        document.getElementById('users_list').appendChild(li);
      }
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

function remove_user(uname) {
  return new Promise(function(resolve, reject) {

    console.log(uname);

    $.ajax({
      url: "http://10.2.1.120:5050/removeUser",
      data: uname,
      dataType : "Text",
      type: "POST",
      contentType: 'application/json',
      success: function () {
        let element = document.getElementById(uname);
        if (element && element.parentNode) {
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
