package error

sealed trait GameError extends Throwable

sealed trait GameBuildingError extends GameError
final case class PlayersOutOfBounds (minPlayers: Int, maxPlayers: Int) extends GameBuildingError {

  override def getMessage = s"There should be between $minPlayers and $maxPlayers players."
}

sealed trait UsernameCreationError extends GameError
final case class InvalidUsernameLength (minLen: Int, maxLen: Int) extends UsernameCreationError {
  
  override def getMessage = s"Username should have $minLen to $maxLen characters."
}
final case object InvalidUsernameCharacters extends UsernameCreationError {

  override def getMessage = "Username can only contain alphanumeric characters."
}

sealed trait PlayerSeqError extends GameError
final case object NoActivePlayers extends PlayerSeqError {

  override def getMessage = "There are no more users playing the game."
}

sealed trait CardPileError extends GameError
final case object CardPileEmpty extends CardPileError {

  override def getMessage = "The card pile is empty."
}
final case object InvalidCardPileIndex extends CardPileError {

  override def getMessage = "Invalid position for card pile."
}