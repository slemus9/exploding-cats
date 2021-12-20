package game.gameserver

import cats.effect.{ExitCode, IO, IOApp}
import org.http4s.blaze.server.BlazeServerBuilder
import scala.concurrent.ExecutionContext
import org.http4s._
import org.http4s.server.websocket.WebSocketBuilder2
import player.domain.Username
import game.gamebuilders.ExplodingCatsBuilder
import fs2.concurrent.SignallingRef
import fs2.concurrent.Signal
import fs2.Stream
import cats.effect.Concurrent
import cats.effect.kernel.Ref
import cats.Functor
import game.gamebuilders.GameBuilder
import cats.effect.kernel.Async

object GameServer extends IOApp {

  // ws://localhost:9002/exploding-cats
  private def httpApp [F[_]: Concurrent] (
    gameState: GameMatch[F]
  ) = (wsb: WebSocketBuilder2[F]) => 
    GameRoute.explodingCatsRoute(gameState, wsb).orNotFound

  def run (args: List[String]): IO[ExitCode] = for {

    gameMatch <- GameMatch.create[IO](ExplodingCatsBuilder)

    _     <- BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(port = 9002, host = "localhost")
      .withHttpWebSocketApp(httpApp(gameMatch))
      .serve
      .compile
      .drain
  } yield ExitCode.Success
}