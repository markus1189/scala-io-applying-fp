package de.codecentric.applyingfp.errorhandling.scala

case class Token(value: String)

class LoginGuard {
  def login(pw: String): Either[LoginGuardError,Token] = {
    ???
  }
}
