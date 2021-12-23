package game.gamebuilders

import card.domain.Card
import player.domain.Username
import game.domain.Game
import error.GameError
import player.domain.Player

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