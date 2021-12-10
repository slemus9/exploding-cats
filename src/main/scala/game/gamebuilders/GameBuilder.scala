package game.gamebuilders

import card.domain.Card
import player.domain.Username
import game.domain.Game
import error.GameError

trait GameBuilder {

  val minNumPlayers: Int
  val maxNumPlayers: Int

  def newGame (usernames: List[Username]): Either[GameError, Game]
}