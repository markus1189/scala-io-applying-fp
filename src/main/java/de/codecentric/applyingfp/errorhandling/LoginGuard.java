package de.codecentric.applyingfp.errorhandling;

public class LoginGuard {
    public void login(String password) {
        if (password.length() < 8) {
            throw new PasswordTooShortException();
        } else if (!password.matches("\\d+")) {
            throw new PasswordContainsNoDigitsException();
        } else if (!password.matches("[!@#$%^&*()_+]")) {
            throw new PasswordContainsNoSpecialCharsException();
        } else {
            System.out.println("You are logged in");
        }
    }
}
