package card.domain

import io.circe.{Decoder, HCursor}
import fs2.Stream
import game.domain.Game
import game.domain.Command._
import game.domain.ServerResponse._
import game.gamestate.WaitPlayerAction
import game.domain.ServerResponse
import game.gamestate.SelectPlayer

sealed trait ActionCard {

  def execute [F[_]] (game: Game): Stream[F, ServerCommand]
}

object ActionCard {

  val cardsByName: Map[String, Card with ActionCard] = Map.from(List(
    Skip,
    Favor,
    Shuffle,
    SeeTheFuture,
    Attack
  ).map { c => c.name -> c })

  implicit val decodeCard = new Decoder[Card with ActionCard] {

    def apply (c: HCursor) = c.get("name") { Decoder[String].emap { card =>
      cardsByName.get(card).toRight(s"Card $card does not exists")
    } }
  }
}

final case object Skip extends Card with ActionCard {

  val name = "Skip"
  val description = "End your turn without drawing a card."

  def execute [F[_]] (game: Game): Stream[F,ServerCommand] = {
    val newPlayers = game.players.moveForward
    Stream(
      UpdateState(WaitPlayerAction(
        game.copy(players = newPlayers)
      )),
      Broadcast(CurrentPlayer(newPlayers))
    )
  } 
}

final case object Favor extends Card with ActionCard {

  val name = "Favor"
  val description = "Force any player to give you a card of their choosing from their hand."

  def execute [F[_]] (game: Game): Stream[F,ServerCommand] = Stream(
    SendResponse(Ok("Type the name of another player to steal their card")),
    UpdateState(SelectPlayer(
      game.players.currentPlayer.username,
      game
    ))
  )
}

final case object Shuffle extends Card with ActionCard {

  val name = "Shuffle"
  val description = "Randomly shuffle the draw pile."

  def execute [F[_]] (game: Game): Stream[F,ServerCommand] = Stream(
    UpdateState(WaitPlayerAction(
      game.copy(drawPile = game.drawPile.shuffle)
    )),
    Broadcast(CurrentPlayer(
      game.players
    ))
  )
}

final case object SeeTheFuture extends Card with ActionCard {

  val name = "SeeTheFuture"
  val description = "View the top three cards of the draw pile privately."

  def execute [F[_]] (game: Game): Stream[F,ServerCommand] = Stream(
    SendResponse(SendCards(
      game.drawPile.viewTop(3)
    )),
    UpdateState(WaitPlayerAction(game)),
    Broadcast(CurrentPlayer(
      game.players
    ))
  )
}

final case object Attack extends Card with ActionCard {

  val name = "Attack"
  val description = "End your turn without drawing a card. The next player has to take two turns."

  def execute [F[_]] (game: Game): Stream[F,ServerCommand] = {
    val newPlayers = game.players.moveForward.updateCurrentPlayer(
      _.copy(numTurns = 2)
    )
    Stream(
      UpdateState(WaitPlayerAction(
        game.copy(players = newPlayers)
      )),
      Broadcast(CurrentPlayer(newPlayers))
    )
  }
}