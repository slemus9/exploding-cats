package player.domain

import card.domain.Card

final case class Player (
  username: Username,
  cardDeck: List[Card]
)