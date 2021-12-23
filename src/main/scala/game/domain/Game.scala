package game.domain

import player.domain.PlayerSeq
import card.domain.CardPile
import player.domain.Username
import card.domain.CardDeck

final case class Game (
  players: PlayerSeq,
  drawPile: CardPile,
  cardDecks: Map[Username, CardDeck]
)