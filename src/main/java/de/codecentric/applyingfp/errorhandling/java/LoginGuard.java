package de.codecentric.applyingfp.errorhandling.java;

public class LoginGuard {
    public Token login(String password) {
        checkLength(password);
        checkDigits(password);
        checkSpecialChars(password);

        return new Token("A&^JD#*&FH");
    }

    private void checkLength(String password) {
        if (password.length() < 8) {
            throw new PasswordTooShortException();
        }
    }

    private void checkDigits(String password) {
        if (!password.matches("\\d+")) {
            throw new PasswordContainsNoDigitsException();
        }
    }

    private void checkSpecialChars(String password) {
        if (!password.matches("[!@#$%^&*()_+]")) {
            throw new PasswordContainsNoSpecialCharsException();
        }
    }
}
