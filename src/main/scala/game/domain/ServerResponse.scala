package game.domain

import card.domain.Card
import player.domain.Username
import io.circe.{Decoder, Encoder, Json}, io.circe.generic.auto._
import io.circe.syntax._

sealed trait ServerResponse
object ServerResponse {

  implicit val encodeResponse = Encoder.instance[ServerResponse] {
    case res: GameIsFull.type => res.asJson
    case res: Ok => res.asJson
    case res: UnexpectedError => res.t.getMessage.asJson
    case res: PlayerDeck => res.asJson
    case res: DrawedCard => res.asJson
    case res: NextCardInPile => res.asJson
    case res: CurrentPlayer => res.asJson
  }

  final case object GameIsFull extends ServerResponse
  final case class Ok (message: String = "OK!") extends ServerResponse
  final case class UnexpectedError (t: Throwable) extends ServerResponse
  final case class PlayerDeck (cards: List[Card]) extends ServerResponse
  final case class DrawedCard (card: Card) extends ServerResponse
  final case class NextCardInPile (card: Card) extends ServerResponse
  final case class CurrentPlayer (username: Username) extends ServerResponse
}
