package game.gamerunners

import game.domain.{
  GamePhase, Start, WaitPlayerAction, ExecPlayerAction,
  WaitPlayerCounterAction, ExecPlayerCounterAction, End,
  GameState
}
import cats.effect.kernel.Sync
import cats.syntax.all._
import cats.Monad

final object GameRunner {

  def next [F[_]: Sync] (p: GamePhase): F[GamePhase] = p match {
    case p @ Start(g)                       => Sync[F].delay(WaitPlayerAction(g))
    case p @ End(_)                         => Sync[F].pure(p)
    case p @ WaitPlayerAction(g)            => 
      if (g.players.length == 1) Sync[F].pure(p)
      else ???
    case p @ ExecPlayerAction(_, _)         => ???
    case p @ WaitPlayerCounterAction(_)     => ???
    case p @ ExecPlayerCounterAction(_, _)  => ???
  }

  def run [F[_]: Sync] (p: GamePhase): F[GameState] = p match {
    case End(g) => Sync[F].pure(g)
    case p @ _  => next(p) >>= run[F]
  }
}