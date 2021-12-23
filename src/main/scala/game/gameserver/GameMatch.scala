package game.gameserver

import player.domain.Username
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame._
import error.{GameError, PlayerNotRegistered, PlayerAlreadyConnected}
import game.domain.ServerResponse
import game.gamestate.GameState
import cats.{ApplicativeError, Functor}
import cats.effect.Concurrent
import cats.effect.std.Queue
import cats.syntax.all._
import game.domain.ServerResponse._
import game.gamebuilders.GameBuilder
import fs2.{Stream, Pipe}
import game.domain.Command._
import cats.effect.kernel.Ref
import io.circe.syntax._
import player.domain.Player

trait GameMatch [F[_]] {

  def getState: F[GameState]

  def addConnection (u: Username, q: GameMatch.Connection[F]) (
    implicit ae: ApplicativeError[F, Throwable]
  ): F[Unit]

  def processCommands (u: Username) (
    implicit ae: ApplicativeError[F, Throwable]
  ): Pipe[F, (PlayerCommand, GameState), Unit]
}
object GameMatch {

  type Connection [F[_]] =
    Queue[F, Option[WebSocketFrame]]

  case class GameMatchState [F[_]] (
    gameState: GameState,
    connections: Map[Username, Connection[F]]
  )
  object GameMatchState {

    def newState [F[_]] (builder: GameBuilder) = GameMatchState(
      GameState.initialState(builder),
      Map.empty[Username, Connection[F]]
    )
  }

  private def emptyMatchState [F[_]: Concurrent] (builder: GameBuilder) = Ref[F].of(
    GameMatchState.newState[F](builder)
  )

  def create [F[_]: Concurrent] (builder: GameBuilder): F[GameMatch[F]] = 
    emptyMatchState(builder).map { stateRef => new GameMatch[F] {

      private def getConnections = stateRef.get.map(_.connections)

      def getState = stateRef.get.map(_.gameState)

      private def expectDisconnected (u: Username) (
        implicit ae: ApplicativeError[F, Throwable]
      ) = getConnections.flatMap { conns => 
        ae.raiseWhen(conns contains u)(
          PlayerAlreadyConnected(u)
        )  
      }

      private def expectConnected (u: Username) (
        implicit ae: ApplicativeError[F, Throwable]
      ) = getConnections.flatMap { conns => 
        ae.raiseUnless(conns contains u)(
          PlayerNotRegistered(u)
        )
      }

      def addConnection (u: Username, q: Connection[F]) (
        implicit ae: ApplicativeError[F, Throwable]
      ): F[Unit] = expectDisconnected(u) >> stateRef.update { s => 
        s.copy(
          connections = s.connections + (u -> q)
        )
      }

      private def updateState (updated: GameState): F[Unit] = 
        stateRef.update { 
          _.copy(gameState = updated)
        }

      private def encodeResponse (res: ServerResponse) = 
        Text(res.asJson.toString)

      private def sendResponse (u: Username, res: ServerResponse): F[Unit] = 
        expectConnected(u) >> getConnections.flatMap { conns => 
          conns(u).offer(encodeResponse(res).some)
        }

      private def sendResponse (messages: Seq[(Username, ServerResponse)]): Stream[F, Unit] = {
        def sendStreams: Seq[Stream[F, Unit]] =
          messages.map { case (u, res) => Stream.eval(sendResponse(u, res)) }
        
        def empty = Stream.empty[F].unitary
        sendStreams.foldLeft(empty)(_ merge _) 
      }

      private def broadcast (res: ServerResponse): Stream[F, Unit] = 
        Stream.eval(getConnections).flatMap { conns =>
          sendResponse(conns.keys.map(_ -> res).toSeq)
        }

      private def closeConnection (u: Username): F[Unit] =
        getConnections.flatMap { conns => 
          conns(u).offer(none)
        }

      private def endConnection (u: Username): F[Unit] = 
        expectConnected(u) >> 
        closeConnection(u) >> 
        stateRef.update { s => 
          s.copy(
            connections = s.connections - u
          )
        }


      def processCommands (u: Username) (
        implicit ae: ApplicativeError[F, Throwable]
      ): Pipe[F, (PlayerCommand, GameState), Unit] = { in => 

        def interpret (cmd: PlayerCommand, s: GameState) =
          s.interpret(u, cmd)

        in.flatMap { case (cmd, s) => s.interpret(u, cmd) }.debug()
          .flatMap {
            case UpdateState(updated) => Stream.eval(updateState(updated))
            case SendResponse(res) => Stream.eval(sendResponse(u, res))
            case Broadcast(message) => broadcast(message)
            case DealCards(players) => sendResponse(
              players.map { case GameBuilder.PlayerSetup(u, cardDeck) => u -> PlayerDeck(cardDeck) }
            )
            case EndConnection => Stream.eval(endConnection(u))
          }
          .handleErrorWith { t =>
            Stream.eval(sendResponse(
              u,
              UnexpectedError(t)
            ))
          }
      }
    }}
}
