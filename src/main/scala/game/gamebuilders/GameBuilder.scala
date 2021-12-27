package game.gamebuilders

import card.domain.Card
import player.domain.{Username, Player}
import game.domain.Game
import error.GameError

trait GameBuilder {

  val minNumPlayers: Int
  val maxNumPlayers: Int

  def newGame (usernames: List[Username]): Either[GameError, GameBuilder.GameSetup]
}

object GameBuilder {

  final case class PlayerSetup (
    username: Username,
    cards:  List[Card]
  )

  final case class GameSetup (
    players: List[PlayerSetup],
    drawPile: List[Card]
  )
}