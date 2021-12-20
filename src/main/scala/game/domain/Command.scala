package game.domain

import card.domain.{ActionCard, Nope}
import player.domain.Username
import io.circe.{Json, Decoder, HCursor}
import io.circe.syntax._
import cats.syntax.option._
import error.{GameError, InvalidCommandName}
import io.circe.Encoder
import game.gameserver.GameState
import player.domain.Player

sealed trait Command
object Command {

  private[domain] def decodeCommandName (cursor: HCursor, cmdName: String) = 
    cursor.get("command") { Decoder[String].emap { name => 
      Either.cond(cmdName == name, name, s"Command '$name' was not expected.")  
    } }

  implicit val decodePlayerCommand: Decoder[PlayerCommand] = Decoder[String].emap {
    case "Connect"  => Right(Connect)
    case "Ready"    => Right(Ready)
    case "DrawCard" => Right(DrawCard)
    case other      => Left(s"Command '$other' was not expected.")
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
  final case class DealCards (players: List[Player]) extends ServerCommand
  final case object EndConnection extends ServerCommand



  sealed trait PlayerCommand extends Command
  final case object Connect extends PlayerCommand {

    val name = "Connect"
  }

  final case object Ready extends PlayerCommand

  final case class InvalidateAction (card: Nope.type) extends PlayerCommand {

    val name = "InvalidateAction"
  }

  sealed trait PlayerActionCommand extends PlayerCommand
  final case object DrawCard extends PlayerActionCommand {

    val name = "DrawCard"
  }
  final case class PlayCard (card: ActionCard) extends PlayerActionCommand {

    val name = "PlayCard"
  }

  sealed trait PlayerLifeOrDeathCommand extends PlayerCommand
  final case object Explode extends PlayerLifeOrDeathCommand {

    val name = "Explode"
  }
  final case object Defused extends PlayerLifeOrDeathCommand {

    val name = "Defused"
  }


  // object PlayerCmdTestJson extends App {

  //   // import PlayerCommand._
  //   import io.circe.syntax._
  //   import io.circe.parser._

  //   val strCmd = 
  //     """
  //       |{
  //       |   "command" : "Explode",
  //       |   "username" : "Player1"
  //       |}""".stripMargin.trim

  //   val jsonCmd = parse(strCmd).getOrElse(Json.Null)

  //   println(jsonCmd.as[Ready])
  // }

}
