function validateForm() {
  var isValid = true;
  var inputs = document.getElementsByTagName("input");
  for (var i = 0; i < inputs.length; i++) {
    if (inputs[i].hasAttribute("required") && inputs[i].value === "") {
      isValid = false;
      inputs[i].classList.add("invalid");
    } else {
      inputs[i].classList.remove("invalid");
    }
  }
  return isValid;
}

$(document).ready(function () {
    document.getElementById('signup_btn').onclick = function (e) {

      let uname = document.getElementById('uname_signup').value;

      if(validateForm()){

        let person = {
          firstName : document.getElementById('name_signup').value,
          lastName : document.getElementById('surname_signup').value,
          username : document.getElementById('uname_signup').value,
          password : document.getElementById('psw_signup').value,
        };

        $.ajax({
          url : "http://10.2.1.120:5050/signup",
          data : JSON.stringify(person),
          type : "POST",
          contentType: 'application/json',
          
          success: function () {
            sessionStorage.setItem("userLog",uname);

            location.href = "./playerMainPage.html"
          },
          error: function(xhr) {
            alert(xhr.responseText)
          }
        })
      }
    }
  });

