package player.domain

final class Username private (val username: String) extends AnyVal

object Username {
  def from (string: String): Either[String, Username] = ???
}