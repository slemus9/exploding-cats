package game.gameserver

import org.http4s._
import org.http4s.dsl.io._
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame._
import org.http4s.server.websocket.WebSocketBuilder2
import cats.effect.Concurrent
import cats.effect.std.Queue
import cats.syntax.all._
import cats.ApplicativeError
import cats.effect.kernel.Ref
import error.{GameError, FrameIsNotText, UnexpectedGameState}
import io.circe.syntax._
import io.circe.parser._
import player.domain.Username
import fs2.{Stream, Pipe}
import fs2.concurrent.SignallingRef
import game.domain.Command._
import game.gamestate.GameState
import cats.effect.kernel.Async

object GameRoute {

  private def filterText [F[_]] (
    implicit ae: ApplicativeError[F, Throwable]
  ): Pipe[F, WebSocketFrame, String] =
    _.evalMap {
      case Text(cmd, _) => ae.pure(cmd)
      case _            => ae.raiseError[String](FrameIsNotText)
    }

  private def decodeCommands [F[_]] (
    implicit ae: ApplicativeError[F, Throwable]
  ): Pipe[F, String, PlayerCommand] =
    _.evalMap { s => ae.fromEither(
      parse(s).flatMap(_.as[PlayerCommand])
    )}
  
  private def buildUsername [F[_]] (username: String) (
    implicit ae: ApplicativeError[F, Throwable]
  ) =
     ae.fromEither(Username.from(username))

  private def processPlayerInput [F[_]] (
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
      _     <- Stream.eval(gameMatch.addConnection(u, q))
      unit  <- in.through(filterText)
        .through(decodeCommands)
        .through(withState)
        .through(gameMatch.processCommands(u))
    } yield unit
  }
  

  def explodingCatsRoute [F[_]: Concurrent] (
    gameMatch: GameMatch[F],
    wsb: WebSocketBuilder2[F]
  ) = HttpRoutes.of[F] {

    case GET -> Root / "exploding-cats" / username => 
      for {

        q <- Queue.synchronous[F, Option[WebSocketFrame]]

        res <- {
          val send = Stream.fromQueueNoneTerminated(q).handleError { t =>
            Text(t.getMessage)
          }

          val receive = processPlayerInput(username, q, gameMatch)

          wsb.build(send, receive)
        }
      } yield res
  }

}