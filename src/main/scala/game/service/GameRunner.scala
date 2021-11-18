package game.service

import scala.annotation.tailrec

import game.domain.GamePhase
import game.domain.WaitAction
import game.domain.End
import game.domain.Start
import game.domain.ExecAction
import game.domain.WaitCounterAction
import game.domain.GameState

import cats.Monad
import cats.effect.kernel.Sync
import cats.syntax.all._

final class GameRunner [F[+_]: Sync] (
  val seedPhase: GamePhase,
  val phaseExec: PhaseExecution[F]
) {
  
  import phaseExec._

  private def nextPhase (phase: GamePhase): F[GamePhase] = phase match {
    case Start(game)              => Sync[F].delay(WaitAction(game))
    case End(game)                => End(game).pure[F]
    case WaitAction(game)         => askPlayerAction.map { ExecAction(game, _) }
    case ExecAction(game, c)      => execPlayerAction(c) as WaitCounterAction(game)
    case WaitCounterAction(game)  => askPlayerCounterAction.map { ExecAction(game, _) }
  }

  private def runPhases (phase: GamePhase): F[GameState] = phase match {
    case End(game)  => game.pure[F]
    case phase @ _  => nextPhase(phase) >>= runPhases
  }

  def run = runPhases(seedPhase)
}