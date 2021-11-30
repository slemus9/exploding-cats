package game.domain

import player.domain.Player
import card.domain.Card

final case class GameState (
  players: List[Player],
  drawPile: List[Card],
  discardPile: List[Card]
)