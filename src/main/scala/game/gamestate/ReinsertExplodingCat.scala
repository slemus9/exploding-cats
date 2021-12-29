package game.gamestate

import game.domain.Game
import cats.ApplicativeError
import cats.effect.Temporal
import game.domain.Command._
import game.domain.ServerResponse._
import player.domain.Username
import card.domain.ExplodingCat
import fs2.Stream
import error.NotThePlayersTurn

final case class ReinsertExplodingCat (
  game: Game
) extends GameState {

  private val Game(players, drawPile, cardDecks) = game

  private def onReinsert [F[_]] (u: Username, index: Int): Stream[F, ServerCommand] = {
    
    val currUsername = players.currentPlayer.username
    if (u != currUsername) GameState.unexpectedError(
      NotThePlayersTurn(currUsername, u)
    ) else {

      val newPlayers = players.moveForward
      drawPile.insertExplodingCat(index)(ExplodingCat).fold(
        GameState.unexpectedError(_),      
        newDrawPile => Stream(
          Broadcast(Ok(s"${u.name} has inserted the Exploding Cat back into the pile")),
          UpdateState(WaitPlayerAction(Game(
            newPlayers,
            newDrawPile,
            cardDecks
          ))),
          Broadcast(CurrentPlayer(newPlayers))
        )  
      )
    }

  }

  def interpret [F[_]: Temporal] (u: Username, cmd: PlayerCommand) (
    implicit ae: ApplicativeError[F,Throwable]
  ): Stream[F, ServerCommand] = cmd match {
    case InsertExplodingCat(index) => onReinsert(u, index)
    case cmd                       => GameState.unexpectedCommand(u, cmd, this)
  }
}