package player.domain

import card.domain.CardDeck

final class Player (
  val username: Username,
  val cardDeck: CardDeck
)