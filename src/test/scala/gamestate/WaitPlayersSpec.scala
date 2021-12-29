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

  "WaitPlayers" - {


    "start match until all players are ready" in {
  
      import utils.RouterUtils._

      val u1 = "player1"
      val u2 = "player2"
      val u3 = "player3"

      val res = for {
        gameMatch <- GameMatch.create[IO](ExplodingCatsBuilder)
        qs <- addConnections(List(u1, u2, u3), gameMatch)
        List(q1, q2, q3) = qs

        pipe1 = processPlayerInput(u1, q1, gameMatch)
        pipe2 = processPlayerInput(u2, q2, gameMatch)
        pipe3 = processPlayerInput(u3, q3, gameMatch)
        out1 = responseStream(q1)
        out2 = responseStream(q2)
        out3 = responseStream(q3)
        out = out1 merge out2 merge out3

        c1 = connect.through(pipe1)
        c2 = connect.through(pipe2)
        c3 = connect.through(pipe3)
        connections = c1 ++ c2 ++ c3

        s1 = readyDelayed[IO](2.seconds).through(pipe1)
        s2 = readyDelayed[IO](1.seconds).through(pipe2)
        s3 = readyDelayed[IO](3.seconds).through(pipe3)
        process = connections ++ (s1 merge s2 merge s3)

        _ <- process.concurrently(out).compile.drain

      } yield gameMatch


      res.flatMap(_.getState).asserting { 

        case WaitPlayerAction(game) => assert(
          players.forall { p => 
            game.cardDecks.keySet.contains(p)  
          }
        )
        case other => assert(false, "State should be WaitPlayerAction")
      }
    }
  }

}