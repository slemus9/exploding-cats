package game.domain

import card.domain.Card
import player.domain.Username

sealed trait ServerCommand

final case class SendPlayerDeck (cards: List[Card]) extends ServerCommand
final case class SendDrawedCard (card: Card) extends ServerCommand
final case class NextCardInPile (card: Card) extends ServerCommand
final case class CurrentPlayer (username: Username) extends ServerCommand