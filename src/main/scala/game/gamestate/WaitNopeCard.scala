package game.gamestate

import game.domain.{Game, Command}
import cats.ApplicativeError
import cats.effect.Temporal
import cats.syntax.all._
import game.domain.Command._
import game.domain.ServerResponse._
import player.domain.Username
import game.domain.ServerResponse
import card.domain.Nope
import error.PlayerDoesNotHaveCard
import card.domain.{Card, ActionCard}
import fs2.Stream
import fs2.concurrent.SignallingRef
import cats.Monad

final case class WaitNopeCard (
  game: Game, 
  actionToInvalidate: Card with ActionCard
) extends GameState {

  private val Game(players, drawPile, cardDecks) = game

  private def onInvalidateAction [F[_]: Temporal] (u: Username): Stream[F, ServerCommand] = {

    Stream.eval(
      GameState.validatePlayerInMap(u, cardDecks)
    ) >> (
      if (!cardDecks(u).contains(Nope)) GameState.unexpectedError(
        PlayerDoesNotHaveCard(Nope)
      )
      else {
        
        val deck = cardDecks(u) - Nope
        val newDecks = cardDecks + (u -> deck)
        val newPlayers = players
          .updateCurrentPlayer(_.copy(
            invalidatedAction = actionToInvalidate.some
          ))

        Stream(
          InterruptCountdown,
          Broadcast(Ok(s"Player ${u.name} used a Nope card")),
          GameState.sendCurrentPlayer(newPlayers),
          UpdateState(WaitPlayerAction(
            Game(newPlayers, drawPile, newDecks)
          ))
        )
      }
    )
  }

  def interpret [F[_]: Temporal] (u: Username, cmd: PlayerCommand) (
    implicit ae: ApplicativeError[F,Throwable]
  ): Stream[F,Command.ServerCommand] = cmd match {
    case InvalidateAction => onInvalidateAction[F](u)
    case cmd              => GameState.unexpectedCommand[F](u, cmd, this)
  }
}