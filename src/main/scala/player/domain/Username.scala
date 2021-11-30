package player.domain

final case class Username private (val username: String) extends AnyVal

object Username {
  def from (string: String): Either[String, Username] = ???
}