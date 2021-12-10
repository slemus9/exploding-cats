package player.domain

import cats.syntax.all._
import error.{GameError, InvalidUsernameLength, InvalidUsernameCharacters}
import io.circe.{Json, Encoder, Decoder}
import io.circe._, io.circe.generic.semiauto._


final case class Username private (val username: String) extends AnyVal

object Username {

  private val minLen = 3
  private val maxLen = 20

  private def validateLen (s: String) = {

    val n = s.length
    // if (n >= minLen && n <= maxLen) s.validNec
    // else InvalidUsernameLength(minLen, maxLen).invalidNec
    Either.cond(
      n >= minLen && n <= maxLen,
      s,
      InvalidUsernameLength(minLen, maxLen)
    )
  }

  private def validateCharacters (s: String) = {

    val onlyAlphanum = s.forall { c => c.isLetter || c.isDigit }
    // if (onlyAlphanum) s.validNec
    // else InvalidUsernameCharacters.invalidNec
    Either.cond(
      onlyAlphanum,
      s,
      InvalidUsernameCharacters
    )
  }

  def from (s: String): Either[GameError, Username] =
    validateLen(s)
      .flatMap(validateCharacters)
      .map(Username(_))

  implicit val encodeUsername = Encoder[String].contramap[Username] {
    case Username(s) => s
  }
  implicit val decodeUsername = Decoder[String].emap {
    Username.from(_).left.map(_.getMessage)
  }
}