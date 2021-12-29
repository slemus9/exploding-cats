package game.gamestate

import game.domain.Game
import game.domain.Command.{PlayerCommand, ServerCommand, SendResponse}
import game.domain.ServerResponse.GameHasEnded
import player.domain.Username
import cats.ApplicativeError
import cats.effect.Temporal
import fs2.Stream

final case class GameFinished (
  game: Game
) extends GameState {

  def interpret [F[_]: Temporal] (u: Username, cmd: PlayerCommand) (
    implicit ae: ApplicativeError[F,Throwable]
  ): Stream[F, ServerCommand] = 
    Stream(SendResponse(GameHasEnded(
      game.players
    )))
}