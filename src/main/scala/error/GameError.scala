package error

import io.circe.DecodingFailure
import game.gamestate.GameState
import player.domain.Username
import game.domain.Command

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

sealed trait CommandParsingError extends GameError
final case object FrameIsNotText extends CommandParsingError {

  override def getMessage = "Server should only receive text"
}
final case class InvalidCommandName (name: String) extends CommandParsingError {

  override def getMessage = s"Command '$name' was not expected"
}

sealed trait GameStateError extends GameError
final case class UnexpectedCommand (gameState: GameState, cmd: Command) extends GameStateError {

  override def getMessage = s"Command ${cmd.getClass.getName} is not valid at the ${gameState.getClass.getName} stage"
}

final case class UnexpectedGameState (found: GameState) extends GameStateError {

  override def getMessage = s"Game state is invalid. Found: ${found.getClass.getName}"
}
final case object GameAlreadyStarted extends GameStateError {

  override def getMessage = s"This game match has already started"
}
final case class PlayerNotRegistered (player: Username) extends GameStateError {

  override def getMessage = s"Player ${player.name} is not registered for this match"
}
final case class PlayerAlreadyConnected (player: Username) extends GameStateError {

  override def getMessage = s"Player ${player.name} is already connected in this match"
}
final case class NotThePlayersTurn (expected: Username, received: Username) extends GameStateError {

  override def getMessage = s"${received.name} it's not your turn. It's ${expected.name}'s turn"
}