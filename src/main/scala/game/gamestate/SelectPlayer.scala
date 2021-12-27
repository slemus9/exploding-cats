package game.gamestate

import player.domain.Username
import game.domain.Game
import game.domain.Command._
import game.domain.ServerResponse._
import cats.ApplicativeError
import cats.effect.Temporal
import fs2.Stream

final case class SelectPlayer (
  sender: Username,
  game: Game
) extends GameState {

  private def onChoosePlayer [F[_]: Temporal] (other: Username): Stream[F, ServerCommand] = {

    Stream.eval(
      game.validatePlayerRegistration(other)
    ) >> Stream(
      Broadcast(Ok(s"Player ${sender} has picked ${other}")),
      SendResponseTo(other, Ok(s"You have to choose a card to give it up to ${sender.name}")),
      UpdateState(AskCard(
        sender,
        other,
        game
      ))
    )
  }

  def interpret [F[_]: Temporal] (u: Username, cmd: PlayerCommand) (
    implicit ae: ApplicativeError[F,Throwable]
  ): Stream[F, ServerCommand] = {

    val resolve = cmd match {
      case ChoosePlayer(other) => ???
      case cmd                 => GameState.unexpectedCommand(u, cmd, this)
    }
    
    Stream.eval(
      game.validatePlayerRegistration(u)
    ) >> resolve
  }
}