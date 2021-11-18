package card.domain

import game.domain.GameState

trait ActionCard {

  def action (gameState: GameState): GameState
}

object CardAction {

  val inaction: ActionCard = identity

}