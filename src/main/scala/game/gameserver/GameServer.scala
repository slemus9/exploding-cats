package game.gameserver

import cats.effect.{ExitCode, IO, IOApp}
import org.http4s.blaze.server.BlazeServerBuilder
import scala.concurrent.ExecutionContext
import org.http4s._
import org.http4s.dsl.io._

object GameServer extends IOApp {
  
  private val explodingCatsRoutes = HttpRoutes.of[IO] {

    case GET -> Root / "exploding-cats" / username => 
      Ok(s"Welcome to exploding cats $username")
  }

  private def httpApp: HttpApp[IO] = 
    explodingCatsRoutes.orNotFound

  def run (args: List[String]): IO[ExitCode] = 
    BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(port = 3600, host = "localhost")
      .withHttpApp(httpApp)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
}