package game.domain

import card.domain.CardPile
import player.domain.Player
import player.domain.PlayerSeq

trait GameState {

  val drawPile: CardPile
  val discardPile: CardPile
  val players: PlayerSeq
}