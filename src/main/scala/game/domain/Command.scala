package game.domain

import card.domain.{Card, ActionCard, Nope}
import card.domain.ActionCard._
import player.domain.Username
import io.circe.{Json, Encoder, Decoder, HCursor}
import io.circe.generic.auto._
import io.circe.syntax._
import cats.syntax.option._
import cats.syntax.functor._
import error.{GameError, InvalidCommandName}
import game.gamestate.GameState
import player.domain.Player
import game.gamebuilders.GameBuilder
import scala.concurrent.duration.FiniteDuration
import fs2.{Stream, Pure}
import card.domain.Attack
import card.domain.SeeTheFuture
import card.domain.CatCard5

sealed trait Command
object Command {

  private[domain] def decodeCommandName (cursor: HCursor, cmdName: String) = 
    cursor.get("command") { Decoder[String].emap { name => 
      Either.cond(cmdName == name, name, s"Command '$name' was not expected.")  
    } }

  private implicit val playCard = new Decoder[PlayCard] {

    def apply (c: HCursor): Decoder.Result[PlayCard] = 
      for {
        _ <- decodeCommandName(c, "PlayCard")
        c <- c.downField("card").as[Card with ActionCard]
      } yield PlayCard(c)
  }

  private implicit val sendCard = new Decoder[SendCard] {

    def apply(c: HCursor): Decoder.Result[SendCard] = 
      for {
        _ <- decodeCommandName(c, "SendCard")
        c <- c.downField("card").as[Card]
      } yield SendCard(c)
  }

  private implicit val reinsertExplodingCat = new Decoder[InsertExplodingCat] {

    def apply(c: HCursor): Decoder.Result[InsertExplodingCat] = 
      c.downField("index")
        .as[Int]
        .map(InsertExplodingCat)
  }

  private implicit val choosePlayer = new Decoder[ChoosePlayer] {

    def apply(c: HCursor): Decoder.Result[ChoosePlayer] = 
      c.downField("username")
        .as[Username]
        .map(ChoosePlayer)
  }

  private val constantDecoder: Decoder[PlayerCommand] = Decoder[String].emap {
      case "Connect"  => Right(Connect)
      case "Ready"    => Right(Ready)
      case "DrawCard" => Right(DrawCard)
      case "NopeCard" => Right(InvalidateAction)
      case other      => Left(s"Command '$other' does not exists.")
    }

  implicit val decodePlayerCommand: Decoder[PlayerCommand] = List(
    Decoder[PlayCard].widen[PlayerCommand],
    Decoder[SendCard].widen[PlayerCommand],
    Decoder[ChoosePlayer].widen[PlayerCommand],
    Decoder[InsertExplodingCat].widen[PlayerCommand],
    constantDecoder
  ).reduce(_ or _)

  implicit val encodePlayerCommand = Encoder.instance[PlayerCommand] {
    case Connect => "Connect".asJson
    case Ready => "Ready".asJson
    case InvalidateAction => "NopeCard".asJson
    case DrawCard => "DrawCard".asJson
    case PlayCard(card) => Json.obj(
      "command" -> "PlayCard".asJson,
      "card"    -> card.asJson
    )
    case cmd: InsertExplodingCat => cmd.asJson
    case SendCard(card) => Json.obj(
      "command" -> "SendCard".asJson,
      "card"    -> card.asJson
    )
    case cmd: ChoosePlayer => cmd.asJson
  }

  sealed trait ServerCommand extends Command
  final case class UpdateState (newGameState: GameState) extends ServerCommand
  final case class SendResponse (response: ServerResponse) extends ServerCommand
  final case class SendResponseTo (u: Username, response: ServerResponse) extends ServerCommand
  final case class Broadcast (message: ServerResponse) extends ServerCommand
  final case class DealCards (players: List[GameBuilder.PlayerSetup]) extends ServerCommand
  final case class StartCountdown (maxWait: FiniteDuration, onFinished: Stream[Pure, ServerCommand]) extends ServerCommand
  final case object InterruptCountdown extends ServerCommand
  final case object EndConnection extends ServerCommand

  sealed trait PlayerCommand extends Command
  final case object Connect extends PlayerCommand
  final case object Ready extends PlayerCommand
  final case object InvalidateAction extends PlayerCommand

  sealed trait PlayerActionCommand extends PlayerCommand
  final case object DrawCard extends PlayerActionCommand 
  final case class PlayCard (card: Card with ActionCard) extends PlayerActionCommand
  final case class InsertExplodingCat (index: Int) extends PlayerActionCommand
  final case class SendCard (card: Card) extends PlayerActionCommand
  final case class ChoosePlayer (username: Username) extends PlayerActionCommand
}

object TestJson extends App {

  val playCard = Encoder[Command.PlayerCommand].apply(
    Command.SendCard(CatCard5)
  )

  println(playCard)
  println(playCard.as[Command.PlayerCommand])
}