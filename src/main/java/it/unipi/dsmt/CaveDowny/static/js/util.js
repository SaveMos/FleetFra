// Used to toggle the menu on smaller screens when clicking on the menu button
function openNav() {
  var x = document.getElementById("navDemo");
  if (x.className.indexOf("w3-show") == -1) {
    x.className += " w3-show";
  } else { 
    x.className = x.className.replace(" w3-show", "");
  }
}

var modal = document.getElementById('signin');
var modal2 = document.getElementById('signup');
// When the user clicks anywhere outside of the modal, close it
window.onclick = function(event) {
    if (event.target == modal2 || event.target == modal) {
        modal2.style.display = "none";
        modal.style.display = "none";
    }
}

document.getElementById('signin').onclick = function () {
  location.href = "./login.html";
};

document.getElementById('signup').onclick = function () {
  location.href = "./signup.html";
};