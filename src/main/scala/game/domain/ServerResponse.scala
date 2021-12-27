package game.domain

import card.domain.Card
import player.domain.PlayerSeq
import io.circe.{Decoder, Encoder, Json}, io.circe.generic.auto._
import io.circe.syntax._
import error.MatchIsFull

sealed trait ServerResponse
object ServerResponse {

  implicit val encodeUnexpectedError = new Encoder[UnexpectedError] {

    def apply (e: UnexpectedError): Json = Json.obj(
      "error" -> e.t.getMessage.asJson
    )
  }

  implicit val encodeResponse = Encoder.instance[ServerResponse] {
    case res: GameIsFull.type => UnexpectedError(MatchIsFull).asJson
    case res: Ok => res.asJson
    case res: UnexpectedError => res.asJson
    case res: SendCards => res.asJson
    case CurrentPlayer(players) => 
      Ok(s"It's ${players.currentPlayer.username.name}'s turn!").asJson
  }

  final case object GameIsFull extends ServerResponse
  final case class Ok (message: String = "OK!") extends ServerResponse
  final case class UnexpectedError (t: Throwable) extends ServerResponse
  final case class SendCards (cards: List[Card]) extends ServerResponse
  final case class CurrentPlayer (players: PlayerSeq) extends ServerResponse
}
