package game.gameserver

import player.domain.Username
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame._
import error.{GameError, PlayerNotRegistered, PlayerAlreadyConnected}
import game.domain.ServerResponse
import game.gamestate.GameState
import cats.{ApplicativeError, Functor}
import cats.effect.Temporal
import cats.effect.std.Queue
import cats.syntax.all._
import game.domain.ServerResponse._
import game.gamebuilders.GameBuilder
import fs2.{Stream, Pipe}
import game.domain.Command._
import cats.effect.kernel.Ref
import io.circe.syntax._
import player.domain.Player
import fs2.concurrent.Signal
import scala.concurrent.duration._
import fs2.concurrent.SignallingRef

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
    connections: Map[Username, Connection[F]],
    interruptCountdown: SignallingRef[F, Boolean]
  )
  object GameMatchState {

    def newState [F[_]: Temporal] (builder: GameBuilder) = 
      SignallingRef[F, Boolean](false).map { signal =>         
        GameMatchState(
          GameState.initialState(builder),
          Map.empty[Username, Connection[F]],
          signal
        )
      }
  }

  private def emptyMatchState [F[_]: Temporal] (builder: GameBuilder) = 
    GameMatchState.newState(builder).flatMap(Ref[F].of)
  // Ref[F].of(
  //   GameMatchState.newState[F](builder)
  // )

  def create [F[_]: Temporal] (builder: GameBuilder): F[GameMatch[F]] = 
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

      private def getInterruptSignal = stateRef.get.map(_.interruptCountdown)

      private def setInterruptSignal (b: Boolean) =
        getInterruptSignal.flatMap(_.set(b))

      private def resetInterruptSignal =
        SignallingRef[F, Boolean](false).flatMap { signal =>
          stateRef.update {
            _.copy(interruptCountdown = signal)
          }  
        }

      private def awaitThenExecute (
        maxWait: FiniteDuration,
        onFinished: Stream[F, ServerCommand]
      ): Stream[F, ServerCommand] = 
        Stream.eval(getInterruptSignal).flatMap { signal => 
          
          Stream
            .awakeEvery[F](1.second)
            .map { d => 
              Broadcast(Ok(s"${maxWait.toSeconds - d.toSeconds}s remaining")) 
            }
            .interruptWhen(signal)
            .interruptAfter(maxWait) ++
            Stream.eval(signal.get).flatMap { interrupted => 
              if (interrupted) Stream.empty
              else onFinished
            }
        }

      private def resolve (
        u: Username, 
        cmd: ServerCommand, 
      ): Stream[F, Unit] = {

        cmd match {
          case UpdateState(updated) => Stream.eval(updateState(updated))
          case SendResponse(res) => Stream.eval(sendResponse(u, res))
          case SendResponseTo(recipient, res) => Stream.eval(sendResponse(recipient, res))
          case Broadcast(message) => broadcast(message)
          case DealCards(players) => sendResponse(
            players.map { case GameBuilder.PlayerSetup(u, cardDeck) => u -> SendCards(cardDeck) }
          )
          case StartCountdown(maxWait, onFinished) => awaitThenExecute(maxWait, onFinished).flatMap { cmd => 
            resolve(u, cmd) 
          } ++ Stream.eval(resetInterruptSignal)
          case InterruptCountdown => Stream.eval(setInterruptSignal(true))
          case EndConnection => Stream.eval(endConnection(u))
        }
      }
      

      def processCommands (u: Username) (
        implicit ae: ApplicativeError[F, Throwable]
      ): Pipe[F, (PlayerCommand, GameState), Unit] = { in => 

        in.flatMap { case (cmd, s) => s.interpret(u, cmd) }.debug()
          .flatMap { cmd => resolve(u, cmd) }
          .handleError(t => t.printStackTrace())
      }
    }}
}
