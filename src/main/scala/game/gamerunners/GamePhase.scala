package game.gamerunners

import cats.syntax.all._
import cats.effect.kernel.Sync
import card.domain.{Card, ActionCard, ExplodingCat}
import player.domain.Username
import error.GameError
import game.gamebuilders.GameBuilder
import game.domain.{
  GameState, PlayerActionCommand, PlayerLifeOrDeathCommand,
  DrawCard, PlayCard, Defused, Explode
}

trait ExecutablePhase {

  def execute [F[_]: Sync]: F[GamePhase]
}

sealed trait GamePhase extends ExecutablePhase

final case class Start (builder: GameBuilder) extends GamePhase {

  def execute [F[_]: Sync]: F[GamePhase] = Sync[F].pure(WaitPlayers(builder))
}


final case class WaitPlayers (builder: GameBuilder) extends GamePhase {

  def execute [F[_]: Sync]: F[GamePhase] = ???
}
final case class SetupGame (builder: GameBuilder, usernames: List[Username]) extends GamePhase {

  override def execute [F[_]: Sync]: F[GamePhase] = {
    
    val game = Sync[F].fromEither(
      builder.newGame(usernames)
    )
    game.map { g => 

      val players = g.players // TODO: Send cards to each player  
      SendCurrentPlayer(GameState.fromGameSetup(g))
    }
  }

}
final case class SendCurrentPlayer (gameState: GameState) extends GamePhase {

  override def execute [F[_]: Sync]: F[GamePhase] = {
    
    val players = gameState.activePlayers

    if (players.size == 1) End.execute
    else {

      val currentPlayer = Sync[F].fromEither(players.currentPlayer)
      val sendCards: F[Unit] = ??? // TODO: Send to all players who's the next player
      sendCards >> currentPlayer.map { 
        WaitPlayerAction(gameState, _)
      }
    }

  }

}
final case class WaitPlayerAction (gameState: GameState, username: Username) extends GamePhase {

  override def execute [F[_]: Sync]: F[GamePhase] = {
    
    def playerAction: F[PlayerActionCommand] = ??? // TODO: Retreive action from player

    playerAction.map {
      case DrawCard       => SendCard(gameState, username)
      case PlayCard(card) => ???
    }
  }

}
final case object WaitNopeCard extends GamePhase {

  override def execute [F[_]: Sync]: F[GamePhase] = ???

}
final case class ExecuteCard (card: ActionCard) extends GamePhase {

  override def execute [F[_]: Sync]: F[GamePhase] = ???

}
final case class SendCard (gameState: GameState, username: Username) extends GamePhase {

  override def execute [F[_]: Sync]: F[GamePhase] = {
    
    val topView = Sync[F].fromEither(
      gameState.drawPile.drawTopCard
    )

    topView.flatMap { case (card, cardPile) => 
        
      if (card.isInstanceOf[ExplodingCat.type]) Sync[F].delay(
        AskDefuseCard(gameState, username)
      )
      else {

        val sendCard: F[Unit] = ??? // send drawed card
        sendCard as AdvanceTurn(gameState)
      }
    }
  }
}
final case class AdvanceTurn (gameState: GameState) extends GamePhase {

  override def execute [F[_]: Sync]: F[GamePhase] = Sync[F].fromEither(
    gameState.activePlayers.moveForward.map { players => SendCurrentPlayer(
      gameState.copy(activePlayers = players)
    ) }
  )
}
final case class AskDefuseCard (gameState: GameState, username: Username) extends GamePhase {

  override def execute [F[_]: Sync]: F[GamePhase] = {
    
    val lifeOrDeath: F[PlayerLifeOrDeathCommand] = ??? // TODO: Ask player if they have a defuse card

    lifeOrDeath.map {
      case Defused => ReinsertExplodingCat(gameState)
      case Explode => KillCurrentPlayer(gameState)
    }
  }

}
final case class ReinsertExplodingCat (gameState: GameState) extends GamePhase {

  override def execute [F[_]: Sync]: F[GamePhase] = {
    
    val index: F[Int] = ??? // ask player for a position
    val cardPile = gameState.drawPile
    index.flatMap { i => 
      val insert = cardPile.insertExplodingCat(i)
      Sync[F].fromEither(insert(ExplodingCat)).map { cardPile =>
        AdvanceTurn(gameState.copy(drawPile = cardPile))
      }
    }
  }

}
final case class KillCurrentPlayer (gameState: GameState) extends GamePhase {

  override def execute [F[_]: Sync]: F[GamePhase] = {

    val removed = Sync[F].fromEither(
      gameState.activePlayers.eliminateCurrentPlayer
    )
    removed.map { players => SendCurrentPlayer(
      gameState.copy(activePlayers = players)
    )}
  }

}
final case object End extends GamePhase {

  override def execute [F[_]: Sync]: F[GamePhase] = 
    Sync[F].pure(End)

}

object GamePhase {

  def run [F[_]: Sync] (p: GamePhase): F[Unit] = p match {
    case End  => Sync[F].unit
    case _    => p.execute >>= run[F]
  }
}