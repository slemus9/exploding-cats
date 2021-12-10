package game.domain

import card.domain.Card
import player.domain.Player

final case class Game (
  players: List[Player],
  drawPile: List[Card],
)