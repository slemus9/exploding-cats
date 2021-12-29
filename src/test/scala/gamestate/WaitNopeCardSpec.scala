package gamestate

import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import cats.effect.testing.scalatest.AsyncIOSpec
import card.domain.SeeTheFuture
import card.domain.Nope
import game.gamebuilders.GameBuilder
import player.domain.Username
import error.{GameError, PlayersOutOfBounds}
import card.domain.ExplodingCat
import game.gameserver.GameMatch
import cats.effect.IO
import game.gamestate.WaitPlayerAction
import org.scalatest.OptionValues
import scala.concurrent.duration._

class WaitNopeCardSpec extends AsyncFreeSpec
  with AsyncIOSpec
  with OptionValues
  with Matchers {

  import utils.RouterUtils._


  val builder = new GameBuilder {

    val minNumPlayers: Int = 2

    val maxNumPlayers: Int = 2

    def newGame (usernames: List[Username]): Either[GameError, GameBuilder.GameSetup] = {
      val players = usernames.map { u => 
        GameBuilder.PlayerSetup(u, List(SeeTheFuture, Nope, Nope))  
      }
      val drawPile = List(ExplodingCat)
      val n = usernames.size

      Either.cond(
        n >= minNumPlayers && n <= maxNumPlayers,
        GameBuilder.GameSetup(players, drawPile),
        PlayersOutOfBounds(minNumPlayers, maxNumPlayers)
      )
    }
  }

  val u1 = "player1"
  val u2 = "player2"
  
  "WaitNopeCard" - {

    "invalidates action" in {

      for {
        gameMatch <- GameMatch.create[IO](builder)
        qs <- addConnections(List(u1, u2), gameMatch)
        List(q1, q2) = qs

        pipe1 = processPlayerInput(u1, q1, gameMatch)
        pipe2 = processPlayerInput(u2, q2, gameMatch)

        out =
          responseStream(q1) merge
          responseStream(q2) 

        connections =
          connect.through(pipe1) ++
          connect.through(pipe2)

        gameStarted =
          ready.through(pipe1) ++
          ready.through(pipe2) 

        process = connections ++ gameStarted ++ (

          playCard(SeeTheFuture).through(pipe1) >>
          invalidateAction.through(pipe2)
        )

        _ <- process.concurrently(out).compile.drain

        a <- gameMatch.getState.asserting {
          case WaitPlayerAction(game) => {

            val currPlayer = game.players.currentPlayer
            assert(
              currPlayer.username.name == u1
            )
            assert(
              currPlayer.invalidatedAction.value == SeeTheFuture
            )
          }
          case other => assert(false, "State should be WaitPlayerAction")
        }

      } yield a
    }

    "allows to stack nope cards" in {

      for {
        gameMatch <- GameMatch.create[IO](builder)
        qs <- addConnections(List(u1, u2), gameMatch)
        List(q1, q2) = qs

        pipe1 = processPlayerInput(u1, q1, gameMatch)
        pipe2 = processPlayerInput(u2, q2, gameMatch)

        out =
          responseStream(q1) merge
          responseStream(q2) 

        connections =
          connect.through(pipe1) ++
          connect.through(pipe2)

        gameStarted =
          ready.through(pipe1) ++
          ready.through(pipe2) 
      

        process = connections ++ gameStarted ++ (
          playCard(SeeTheFuture).through(pipe1) >>
          invalidateAction.through(pipe2)
        ) ++ invalidateAction.through(pipe1)

        _ <- process.concurrently(out).compile.drain

        a <- gameMatch.getState.asserting {
          case WaitPlayerAction(game) => {

            val currPlayer = game.players.currentPlayer
            assert(
              currPlayer.username.name == u1
            )
            assert(
              currPlayer.invalidatedAction.isEmpty          
            )
          }
          case other => assert(false, "State should be WaitPlayerAction")
        }

      } yield a
    }
  }

}