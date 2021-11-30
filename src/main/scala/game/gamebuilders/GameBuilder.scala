package game.gamebuilders

import card.domain.Card
import player.domain.Username
import game.domain.GameState

trait GameBuilder {

  def newGame (userNames: List[Username]): Either[GameBuildingError, GameState]
}