package player.domain

import card.domain.{Card, ActionCard}
import io.circe._, io.circe.generic.semiauto._
import game.gamebuilders.GameBuilder
import card.domain.CardDeck

final case class Player private (
  username: Username,
  invalidatedAction: Option[Card with ActionCard] = None,
  numTurns: Int = 1
)