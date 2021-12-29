package utils

import game.domain.Command._
import game.gameserver.GameMatch
import game.gamestate.GameState
import player.domain.Username
import error.FrameIsNotText
import cats.ApplicativeError
import cats.effect.{Temporal, Concurrent}
import cats.effect.std.Queue
import cats.syntax.all._
import io.circe.syntax._
import io.circe.parser._
import fs2.{Stream, Pipe}
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame.Text
import scala.concurrent.duration._
import cats.Functor
import cats.MonadError
import card.domain.{Card, ActionCard}

object RouterUtils {

private def filterText [F[_]] (
    implicit ae: ApplicativeError[F, Throwable]
  ): Pipe[F, WebSocketFrame, String] =
    _.evalMap {
      case Text(cmd, _) => ae.pure(cmd)
      case _            => ae.raiseError[String](FrameIsNotText)
    }

  private def decodePlayerCommands [F[_]] (
    implicit ae: ApplicativeError[F, Throwable]
  ): Pipe[F, String, PlayerCommand] =
    _.evalMap { s => ae.fromEither(
      parse(s).flatMap(_.as[PlayerCommand])
    )}
  
  private def buildUsername [F[_]] (username: String) (
    implicit ae: ApplicativeError[F, Throwable]
  ) =
     ae.fromEither(Username.from(username))

  def addConnections [F[_]: Concurrent] (
    names: List[String],
    gameMatch: GameMatch[F]
  ) (
    implicit ae: MonadError[F, Throwable]
  ) = 
    for {
      usernames <- names.traverse(buildUsername(_))
      out <- usernames.traverse { u => 
        Queue.synchronous[F, Option[WebSocketFrame]].flatMap { q =>
          gameMatch.addConnection(u, q).as(q)
        }
      }
    } yield out

  def processPlayerInput [F[_]] (
    usernameStr: String,
    q: Queue[F,Option[WebSocketFrame]],
    gameMatch: GameMatch[F]
  ) (
    implicit ae: ApplicativeError[F, Throwable]
  ): Pipe[F, WebSocketFrame, Unit] = { in =>

    def withState: Pipe[F, PlayerCommand, (PlayerCommand, GameState)] =
      _.evalMap { cmd => gameMatch.getState.map(cmd -> _) }

    for {
      u     <- Stream.eval(buildUsername(usernameStr))
      // _     <- Stream.eval(gameMatch.addConnection(u, q))
      unit  <- in.through(filterText)
        .through(decodePlayerCommands)
        .through(withState)
        .through(gameMatch.processCommands(u))
    } yield unit
  }

  def responseStream [F[_]: Functor] (
    q: Queue[F,Option[WebSocketFrame]]
  ) = Stream.fromQueueNoneTerminated(q)

  def encode (cmd: PlayerCommand) =
    Text(cmd.asJson.toString)

  def sleep [F[_]: Temporal] (delay: FiniteDuration) =
    Stream.sleep[F](delay)

  def ready [F[_]] = Stream(encode(Ready))

  def readyDelayed [F[_]: Temporal] (
    delay: FiniteDuration
  ) = 
    sleep(delay) >> ready

  def connect [F[_]] = Stream(encode(Connect))

  def connectDelayed [F[_]: Temporal] (
    delay: FiniteDuration
  ) = 
    sleep(delay) >> connect

  def drawCard [F[_]] = Stream(encode(DrawCard))

  def playCard (card: Card with ActionCard) =
    Stream(encode(PlayCard(card)))

  def reinsertExplodingCat (index: Int) = 
    Stream(encode(InsertExplodingCat(index)))
}