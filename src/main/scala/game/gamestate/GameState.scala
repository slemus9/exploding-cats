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
import cats.effect.Temporal
import error.UnexpectedCommand
import fs2.Stream
import player.domain.Player
import card.domain.CardDeck
import error.PlayerNotRegistered
import player.domain.PlayerSeq


trait GameState {

  def interpret [F[_]: Temporal] (u: Username, cmd: PlayerCommand) (
    implicit ae: ApplicativeError[F, Throwable]
  ): Stream[F, ServerCommand]
}

object GameState {

  def initialState [F[_]] (builder: GameBuilder): GameState =
    WaitPlayers(builder, none, Map.empty)

  private[gamestate] def unexpectedError [F[_]] (
    t: Throwable
  ): Stream[F, ServerCommand] = Stream(SendResponse(UnexpectedError(t)))

  private[gamestate] def unexpectedCommand [F[_]] (
    username: Username, 
    cmd: PlayerCommand, 
    gameState: GameState
  ): Stream[F, ServerCommand] = unexpectedError(
    UnexpectedCommand(gameState, cmd)
  )
}