function toggleForms() {
    const loginForm = document.getElementById('login-form');
    const signupForm = document.getElementById('signup-form');
    if (loginForm.style.display === 'none') {
        loginForm.style.display = 'block';
        signupForm.style.display = 'none';
    } else {
        loginForm.style.display = 'none';
        signupForm.style.display = 'block';
    }
}
document.addEventListener('DOMContentLoaded', () => {
    const inputs = document.querySelectorAll('input');
    inputs.forEach(input => {
        input.addEventListener('focus', () => {
            input.placeholder = '';
        });
        input.addEventListener('blur', () => {
            if (input.id === 'login-username') input.placeholder = 'Enter your username';
            if (input.id === 'login-password') input.placeholder = 'Enter your password';
            if (input.id === 'signup-name') input.placeholder = 'Enter your name';
            if (input.id === 'signup-surname') input.placeholder = 'Enter your surname';
            if (input.id === 'signup-username') input.placeholder = 'Choose a username';
            if (input.id === 'signup-email') input.placeholder = 'Enter your email';
            if (input.id === 'signup-password') input.placeholder = 'Create a password';
        });
    });
});

document.querySelector('button[class="login"]').addEventListener('click', (e) => {
    e.preventDefault();
    let hasError = false;

    // Reset all error messages
    document.querySelectorAll('.error-message').forEach(msg => msg.style.display = 'none');

    // Username validation
    const username = document.getElementById('login-username').value.trim();
    if (username.length === 0) {
        document.getElementById('error-login-username').style.display = 'block';
        hasError = true;
    }

    // Password validation
    const password = document.getElementById('login-password').value.trim();
    if (password.length === 0) {
        document.getElementById('error-login-password').style.display = 'block';
        hasError = true;
    }

    // If no errors, submit the form (this is where you can handle the submission logic)
    if (!hasError) {
        console.log('Form submitted successfully!');
    }
});
document.querySelector('button[class="signup"]').addEventListener('click', (e) => {
    e.preventDefault();
    let hasError = false;

    // Reset all error messages
    document.querySelectorAll('.error-message').forEach(msg => msg.style.display = 'none');

    // Name validation
    const name = document.getElementById('signup-name').value.trim();
    if (name.length < 3 || /[^a-zA-Z]/.test(name)) {
        document.getElementById('error-signup-name').style.display = 'block';
        hasError = true;
    }

    // Surname validation
    const surname = document.getElementById('signup-surname').value.trim();
    if (surname.length < 3 || /[^a-zA-Z]/.test(surname)) {
        document.getElementById('error-signup-surname').style.display = 'block';
        hasError = true;
    }

    // Username validation
    const username = document.getElementById('signup-username').value.trim();
    if (username.length < 5) {
        document.getElementById('error-signup-username').style.display = 'block';
        hasError = true;
    }

    // Email validation
    const email = document.getElementById('signup-email').value.trim();
    const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    if (!emailRegex.test(email)) {
        document.getElementById('error-signup-email').style.display = 'block';
        hasError = true;
    }

    // Password validation
    const password = document.getElementById('signup-password').value.trim();
    if (password.length < 5) {
        document.getElementById('error-signup-password').style.display = 'block';
        hasError = true;
    }

    // If no errors, submit the form (this is where you can handle the submission logic)
    if (!hasError) {
        console.log('Form submitted successfully!');
    }
});
