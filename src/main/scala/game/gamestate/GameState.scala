package game.gamestate

import player.domain.Username
import game.domain.Command.{
  ServerCommand, PlayerCommand, 
  SendResponse, Broadcast
}
import game.domain.ServerResponse.{Ok, UnexpectedError}
import game.gamebuilders.GameBuilder
import cats.ApplicativeError
import cats.syntax.option._
import error.UnexpectedCommand
import fs2.Stream


trait GameState {

  def interpret [F[_]] (u: Username, cmd: PlayerCommand) (
    implicit ae: ApplicativeError[F, Throwable]
  ): Stream[F, ServerCommand]
}

object GameState {

  def initialState (builder: GameBuilder): GameState =
    WaitPlayers(builder, none, Map.empty)

  def unexpectedError [F[_]] (
    t: Throwable
  ) = Stream(SendResponse(UnexpectedError(t)))

  def unexpectedCommand [F[_]] (
    username: Username, 
    cmd: PlayerCommand, 
    gameState: GameState
  ): Stream[F, ServerCommand] = unexpectedError(
    UnexpectedCommand(gameState, cmd)
  )

  def sendCurrentPlayer (username: Username) =
    Broadcast(Ok(s"It's ${username.name}'s turn!"))
}