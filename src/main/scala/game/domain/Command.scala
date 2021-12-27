package game.domain

import card.domain.{Card, ActionCard, Nope}
import card.domain.ActionCard._
import player.domain.Username
import io.circe.{Json, Decoder, HCursor}
import io.circe.syntax._
import cats.syntax.option._
import cats.syntax.functor._
import error.{GameError, InvalidCommandName}
import io.circe.Encoder
import game.gamestate.GameState
import player.domain.Player
import game.gamebuilders.GameBuilder
import scala.concurrent.duration.FiniteDuration

sealed trait Command
object Command {

  private[domain] def decodeCommandName (cursor: HCursor, cmdName: String) = 
    cursor.get("command") { Decoder[String].emap { name => 
      Either.cond(cmdName == name, name, s"Command '$name' was not expected.")  
    } }

  private implicit val playCard = new Decoder[PlayCard] {

    def apply (c: HCursor): Decoder.Result[PlayCard] = 
      c.downField("card")
        .as[Card with ActionCard]
        .map(PlayCard)

  }

  implicit val decodePlayerCommand: Decoder[PlayerCommand] = 
    Decoder[PlayCard].widen or 
    Decoder[String].emap {
      case "Connect"  => Right(Connect)
      case "Ready"    => Right(Ready)
      case "DrawCard" => Right(DrawCard)
      case "NopeCard" => Right(InvalidateAction)
      case other      => Left(s"Command '$other' does not exists.")
    }

  implicit val encodePlayerCommand: Encoder[PlayerCommand] = Encoder[String].contramap[PlayerCommand] {
    case Connect  => "Connect"
    case Ready    => "Ready"
    case DrawCard => "DrawCard"
  }

  sealed trait ServerCommand extends Command
  final case class UpdateState (newGameState: GameState) extends ServerCommand
  final case class SendResponse (response: ServerResponse) extends ServerCommand
  final case class Broadcast (message: ServerResponse) extends ServerCommand
  final case class DealCards (players: List[GameBuilder.PlayerSetup]) extends ServerCommand
  final case class StartCountdown (maxWait: FiniteDuration, onFinished: ServerCommand) extends ServerCommand
  final case object InterruptCountdown extends ServerCommand
  final case object EndConnection extends ServerCommand



  sealed trait PlayerCommand extends Command
  final case object Connect extends PlayerCommand {

    val name = "Connect"
  }

  final case object Ready extends PlayerCommand

  final case object InvalidateAction extends PlayerCommand {

    val name = "InvalidateAction"
  }

  sealed trait PlayerActionCommand extends PlayerCommand
  final case object DrawCard extends PlayerActionCommand {

    val name = "DrawCard"
  }
  final case class PlayCard (card: Card with ActionCard) extends PlayerActionCommand {

    val name = "PlayCard"
  }

  sealed trait PlayerLifeOrDeathCommand extends PlayerCommand
  final case object Explode extends PlayerLifeOrDeathCommand {

    val name = "Explode"
  }
  final case object Defused extends PlayerLifeOrDeathCommand {

    val name = "Defused"
  }
}
