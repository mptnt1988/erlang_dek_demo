function validateForm() {
    var uname = $("#uname").val();
    var dname = $("#dname").val();
    var password = $("#password").val();
    var cpassword = $("#cpassword").val();
    var result = false;
    if (uname == '' || dname == '' || password == '' || cpassword == '') {
        alert("Please fill all fields.");
    }
    else if ((password.length) < 1) {
        alert("Password should be at least 1 character in length.");
    }
    else if (!(password).match(cpassword)) {
        alert("Your passwords don't match. Try again!");
    }
    else result = true;
    return result
}
