package de.codecentric.applyingfp.errorhandling.scala

sealed trait LoginGuardError
case object PasswordTooShort extends LoginGuardError
case object PasswordContainsNoDigits extends LoginGuardError
case object PasswordContainsNoSpecialChars extends LoginGuardError