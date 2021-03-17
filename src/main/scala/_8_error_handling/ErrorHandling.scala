package net.savinko
package _8_error_handling

import cats.data.ValidatedNec
import cats.syntax.all._

import java.time.Instant
import scala.util.Try

// Homework. Place the solution under `error_handling` package in your homework repository.
//
// 1. Model `PaymentCard` class as an ADT (protect against invalid data as much as it makes sense).
// 2. Add `ValidationError` cases (at least 5, may be more).
// 3. Implement `validate` method to construct `PaymentCard` instance from the supplied raw data.
object ErrorHandling {

  case class PaymentCard(
                          val name: String,
                          val number: String,
                          val expirationDate: String,
                          val code: String
                        )

  sealed trait ValidationError

  object ValidationError {

    final case object InvalidCardHolderName extends ValidationError {
      override def toString: String = "Username must be between 3 and 30 characters"
    }

    final case object InvalidCardNumber extends ValidationError {
      override def toString: String = "Card number must be 16 digit length."
    }

    final case object ExpDateIsExpired extends ValidationError {
      override def toString: String = "Expiration date has been expired."
    }

    final case object InvalidExpDate extends ValidationError {
      override def toString: String = "Invalid expiration date."
    }

    final case object InvalidSecurityCode extends ValidationError {
      override def toString: String = "Invalid security code."
    }
  }

  object PaymentCardValidator {

    import ValidationError._

    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    def validate(
                  name: String,
                  number: String,
                  expirationDate: String,
                  securityCode: String,
                ): AllErrorsOr[PaymentCard] =
      (
        validateName(name),
        validateNumber(number),
        validateExpDate(expirationDate),
        validateCode(securityCode),
      ).mapN(PaymentCard)

    private def validateName(name: String): AllErrorsOr[String] =
      if ((3 to 30).contains(name.length)) name.validNec
      else InvalidCardHolderName.invalidNec

    private def validateNumber(number: String): AllErrorsOr[String] = {
      val onlyDigits = number.toLongOption

      if (onlyDigits.isDefined && number.length == 16) number.validNec
      else InvalidCardNumber.invalidNec
    }

    private def validateExpDate(expirationDate: String): AllErrorsOr[String] = {
      val res = Try {
        expirationDate.toLongOption.map(Instant.ofEpochMilli)
      }.getOrElse(None)

      res.map { expDate =>
        if (expDate.isBefore(Instant.now())) expirationDate.validNec
        else ExpDateIsExpired.invalidNec
      }.getOrElse(InvalidExpDate.invalidNec)
    }

    private def validateCode(code: String): AllErrorsOr[String] =
      if (code.length == 3) code.validNec
      else InvalidSecurityCode.invalidNec
  }
}
