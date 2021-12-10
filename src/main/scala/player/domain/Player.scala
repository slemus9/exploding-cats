package player.domain

import card.domain.Card
import io.circe._, io.circe.generic.semiauto._

final case class Player (
  username: Username,
  cardDeck: List[Card]
)

object Player {

  implicit val encodePlayer = deriveEncoder[Player]
  implicit val decodePlayer = deriveDecoder[Player]
}