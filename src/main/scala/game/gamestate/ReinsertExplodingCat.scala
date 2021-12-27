package game.gamestate

import game.domain.Game
import cats.ApplicativeError
import cats.effect.Temporal
import game.domain.Command._
import game.domain.ServerResponse._
import player.domain.Username
import card.domain.ExplodingCat
import fs2.Stream

final case class ReinsertExplodingCat (
  game: Game
) extends GameState {

  private val Game(players, drawPile, cardDecks) = game

  private def onReinsert [F[_]] (u: Username, index: Int): Stream[F, ServerCommand] = {
    
    val newPlayers = players.moveBackwards
    drawPile.insertExplodingCat(index)(ExplodingCat).fold(
      GameState.unexpectedError(_),      
      newDrawPile => Stream(
        Broadcast(Ok(s"${u.name} has inserted the Exploding Cat back into the pile")),
        UpdateState(WaitPlayerAction(Game(
          newPlayers,
          newDrawPile,
          cardDecks
        ))),
        GameState.sendCurrentPlayer(
          newPlayers
        )
      )  
    )
  }

  def interpret [F[_]: Temporal] (u: Username, cmd: PlayerCommand) (
    implicit ae: ApplicativeError[F,Throwable]
  ): fs2.Stream[F, ServerCommand] = cmd match {
    case InsertExplodingCat(index) => onReinsert(u, index)
    case cmd                       => GameState.unexpectedCommand[F](u, cmd, this)
  }
}