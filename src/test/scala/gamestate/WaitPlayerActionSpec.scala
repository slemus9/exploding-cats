package gamestate

import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.effect.IO
import cats.effect.std.Queue
import game.gameserver.GameMatch
import game.gamebuilders.ExplodingCatsBuilder
import org.http4s.websocket.WebSocketFrame
import game.gamestate.WaitPlayerAction
import scala.concurrent.duration._
import game.gamebuilders.GameBuilder
import error.GameError
import player.domain.Username
import card.domain.Card._
import card.domain.ActionCard._
import error.PlayersOutOfBounds
import fs2.Stream
import card.domain.CatCard1
import game.gamestate.GameFinished
import card.domain.ExplodingCat
import card.domain.Defuse
import card.domain.SeeTheFuture

class WaitPlayerActionSpec extends AsyncFreeSpec
  with AsyncIOSpec
  with Matchers {


  "WaitPlayersSpec" - {

    import utils.RouterUtils._


    "change to the next turn when a card is drawed" in {

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

        out =
          responseStream(q1) merge
          responseStream(q2) merge
          responseStream(q3)

        connections =
          connect.through(pipe1) ++
          connect.through(pipe2) ++
          connect.through(pipe3)

        gameStarted =
          ready.through(pipe1) ++
          ready.through(pipe2) ++
          ready.through(pipe3)
        
        process = connections ++ gameStarted ++ drawCard.through(pipe1)

        _ <- process.concurrently(out).compile.drain

      } yield gameMatch

      res.flatMap(_.getState).asserting {
        case WaitPlayerAction(game) => {
          assert(game.players.currentPlayer.username.name == u2)
        }
        case other => assert(false, "State should be WaitPlayerAction")
      }
    }

    "ends game if there is only one player" in {
  
      val builder = new GameBuilder {

        val minNumPlayers: Int = 2

        val maxNumPlayers: Int = 2

        def newGame (usernames: List[Username]): Either[GameError,GameBuilder.GameSetup] = {
          val players = usernames.map { u => 
            GameBuilder.PlayerSetup(u, List(CatCard1))  
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

        processs = connections ++ gameStarted ++ drawCard.through(pipe1)

        _ <- processs.concurrently(out).compile.drain

        a <- gameMatch.getState.asserting {
          case GameFinished(game) => assert(
            game.players.currentPlayer.username.name == u2
          )
          case other => assert(false, "State should be GameFinished")
        }

      } yield a
    }

    "advances 1 turn if player draws a non-exploding card" in {
      
      val u1 = "player1"
      val u2 = "player2"
      for {
        gameMatch <- GameMatch.create[IO](ExplodingCatsBuilder)
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

        processs = connections ++ gameStarted ++ drawCard.through(pipe1)

        _ <- processs.concurrently(out).compile.drain
        
        a <- gameMatch.getState.asserting {
          case WaitPlayerAction(game) => assert(
            game.players.currentPlayer.username.name == u2
          )
          case other => assert(false, "State should be WaitPlayerAction")
        }
      } yield a
    }

    "eliminates a player when drawing a Exploding Cat without a Defuse card" in {

      val builder = new GameBuilder {

        val minNumPlayers: Int = 4

        val maxNumPlayers: Int = 4

        def newGame (usernames: List[Username]): Either[GameError,GameBuilder.GameSetup] = {
          val players = usernames.map { u => 
            GameBuilder.PlayerSetup(u, List(CatCard1))  
          }
          val drawPile = List.fill(3)(ExplodingCat)
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
      val u3 = "player3"
      val u4 = "player4"

      for {
        gameMatch <- GameMatch.create[IO](builder)
        qs <- addConnections(List(u1, u2, u3, u4), gameMatch)
        List(q1, q2, q3, q4) = qs

        pipe1 = processPlayerInput(u1, q1, gameMatch)
        pipe2 = processPlayerInput(u2, q2, gameMatch)
        pipe3 = processPlayerInput(u3, q3, gameMatch)
        pipe4 = processPlayerInput(u4, q4, gameMatch)

        out =
          responseStream(q1) merge
          responseStream(q2) merge
          responseStream(q3) merge
          responseStream(q4) 

        connections =
          connect.through(pipe3) ++
          connect.through(pipe4) ++
          connect.through(pipe1) ++
          connect.through(pipe2)

        gameStarted =
          ready.through(pipe3) ++
          ready.through(pipe4) ++
          ready.through(pipe1) ++
          ready.through(pipe2) 

        process = connections ++ gameStarted ++ drawCard.through(pipe3)

        _ <- process.concurrently(out).compile.drain

        a <- gameMatch.getState.asserting {
          case WaitPlayerAction(game) => {
            val players = game.players
            assert(players.size == 3)
            assert(
              players.currentPlayer.username.name == u4
            )
          }
          case other => assert(false, "State should be WaitPlayerAction")
        }
      } yield a
    }

    "defuses exploding cat if drawed and advances 1 turn" in {

      val builder = new GameBuilder {

        val minNumPlayers: Int = 4

        val maxNumPlayers: Int = 4

        def newGame (usernames: List[Username]): Either[GameError,GameBuilder.GameSetup] = {
          val players = usernames.map { u => 
            GameBuilder.PlayerSetup(u, List(Defuse))  
          }
          val drawPile = List(ExplodingCat, SeeTheFuture, ExplodingCat, ExplodingCat)
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
      val u3 = "player3"
      val u4 = "player4"

      for {
        gameMatch <- GameMatch.create[IO](builder)
        qs <- addConnections(List(u1, u2, u3, u4), gameMatch)
        List(q1, q2, q3, q4) = qs

        pipe1 = processPlayerInput(u1, q1, gameMatch)
        pipe2 = processPlayerInput(u2, q2, gameMatch)
        pipe3 = processPlayerInput(u3, q3, gameMatch)
        pipe4 = processPlayerInput(u4, q4, gameMatch)

        out =
          responseStream(q1) merge
          responseStream(q2) merge
          responseStream(q3) merge
          responseStream(q4) 

        connections =
          connect.through(pipe3) ++
          connect.through(pipe4) ++
          connect.through(pipe1) ++
          connect.through(pipe2)

        gameStarted =
          ready.through(pipe3) ++
          ready.through(pipe4) ++
          ready.through(pipe1) ++
          ready.through(pipe2) 

        process = connections ++ 
          gameStarted ++ 
          drawCard.through(pipe3) ++
          reinsertExplodingCat(0).through(pipe3)

        _ <- process.concurrently(out).compile.drain

        a <- gameMatch.getState.asserting {
          case WaitPlayerAction(game) => {
            val players = game.players
            val drawPile = game.drawPile
            assert(players.size == 4)
            assert(
              players.currentPlayer.username.name == u4
            )
            assert(
              drawPile.drawTopCard
                .getOrElse(sys.error("Pile should not be empty"))
                ._1 == ExplodingCat
            )
          }
          case other => assert(false, "State should be WaitPlayerAction")
        }
      } yield a
    }

    "play unlimited number of cards before drawing one" in {

      val builder = new GameBuilder {

        val minNumPlayers: Int = 2

        val maxNumPlayers: Int = 2

        def newGame (usernames: List[Username]): Either[GameError,GameBuilder.GameSetup] = {
          val players = usernames.map { u => 
            GameBuilder.PlayerSetup(u, List.fill(5)(SeeTheFuture))  
          }
          val drawPile = List(SeeTheFuture, ExplodingCat, Defuse, SeeTheFuture)
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

        repeatPlay =
          Stream.awakeEvery[IO](7.seconds)
            .flatMap(_ => playCard(SeeTheFuture))
            .interruptAfter(14.seconds)

        playCards = connections ++ gameStarted ++ 
          playCard(SeeTheFuture).through(pipe1) ++
          repeatPlay.through(pipe1)

        _ <- playCards.concurrently(out).compile.drain

        a <- gameMatch.getState.asserting {
          case WaitPlayerAction(game) => assert(
            game.players.currentPlayer.username.name == u1
          )
          case other => assert(false, "State should be WaitPlayerAction")
        }

      } yield a
    }
  }
}