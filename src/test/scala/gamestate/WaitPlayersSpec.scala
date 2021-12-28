package gamestate

import org.scalatest.matchers.should.Matchers
import org.scalatest.freespec.AsyncFreeSpec
import player.domain.Username
import game.domain.Command._
import game.gamestate.GameState
import game.gameserver.GameMatch
import game.gamebuilders.ExplodingCatsBuilder
import error.FrameIsNotText
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame.Text
import io.circe.syntax._
import io.circe.parser._
import cats.ApplicativeError
import cats.effect.std.Queue
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.effect._
import cats.syntax.all._
import scala.concurrent.duration._
import fs2.{Stream, Pipe}
import game.gamestate.WaitPlayerAction

class WaitPlayersSpec extends AsyncFreeSpec
  with AsyncIOSpec
  with Matchers {

  private val players = List.range(1, 4).map { i => 
    Username.from(s"player$i")
  }.collect {
    case Right(u) => u
  }

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

  private def processPlayerInput /* [F[_]] */ (
    u: Username,
    q: Queue[IO,Option[WebSocketFrame]],
    gameMatch: GameMatch[IO]
  ) (
    // implicit ae: ApplicativeError[F, Throwable]
  ): Pipe[IO, WebSocketFrame, Unit] = { in =>

    def withState: Pipe[IO, PlayerCommand, (PlayerCommand, GameState)] =
      _.evalMap { cmd => gameMatch.getState.map(cmd -> _) }

    for {
      _     <- Stream.eval(gameMatch.addConnection(u, q))
      unit  <- in.through(filterText)
        .through(decodeCommands)
        .through(withState)
        .through(gameMatch.processCommands(u))
    } yield unit
  }

  "WaitPlayers" - {


    "start match until all players are ready" in {
  
      def ready (
        u: Username,
        delay: FiniteDuration
      ) = 
        Stream.sleep[IO](delay).flatMap { _ =>
          Stream.eval(IO.println(s"${u.name} getting ready"))  
        }.as(Text(""""Ready""""))
  
      def connect (
        u: Username,
        delay: FiniteDuration
      ) = 
        Stream.sleep[IO](delay).flatMap { _ => 
          Stream.eval(IO.println(s"Connecting ${u.name}"))  
        }.as(Text(""""Connect""""))

      val List(u1, u2, u3) = players

      val res = for {
        gameMatch <- GameMatch.create[IO](ExplodingCatsBuilder)
        q1 <- Queue.unbounded[IO, Option[WebSocketFrame]]
        q2 <- Queue.unbounded[IO, Option[WebSocketFrame]]
        q3 <- Queue.unbounded[IO, Option[WebSocketFrame]]

        pipe1 = processPlayerInput(u1, q1, gameMatch)
        pipe2 = processPlayerInput(u2, q2, gameMatch)
        pipe3 = processPlayerInput(u3, q3, gameMatch)
        val c1 = connect(u1, 1.seconds).append(ready(u1, 6.seconds)).through(pipe1)
        val c2 = connect(u2, 2.seconds).append(ready(u2, 5.seconds)).through(pipe2)
        val c3 = connect(u3, 3.seconds).append(ready(u3, 4.seconds)).through(pipe3)

        process = c1 merge c2 merge c3

        _ <- process.compile.drain



      } yield gameMatch


      res.flatMap(_.getState).asserting { 

        case WaitPlayerAction(game) => assert(
          players.forall { p => 
            game.cardDecks.keySet.contains(p)  
          }
        )
        case other => assert(false)
      }
    }
  }

}