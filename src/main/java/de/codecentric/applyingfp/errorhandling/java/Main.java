package de.codecentric.applyingfp.errorhandling.java;

public class Main {
    public static void main(String[] args) {
        LoginGuard guard = new LoginGuard();
        try {

            guard.login("joe");
            guard.login("joesmith");
            guard.login("joesmith1");
            guard.login("joesmith11!!11!!!1");

        } catch (LoginGuardException ex) {
            if (ex instanceof PasswordTooShortException) {
                System.out.println("Your password is too short!");
            } else if (ex instanceof PasswordContainsNoDigitsException) {
                System.out.println("Your password must contain at least one digit!");
            } else if (ex instanceof PasswordContainsNoSpecialCharsException) {
                System.out.println("Your password must contain at least one special char!");
            }
        }
    }
}
