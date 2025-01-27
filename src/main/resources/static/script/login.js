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