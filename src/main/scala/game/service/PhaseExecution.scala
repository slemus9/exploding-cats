package game.service

import cats.effect.kernel.Sync
import card.domain.ActionCard
import game.domain.GameState
import card.domain.NopeCard

final abstract class PhaseExecution [F[_]: Sync] {

  def askPlayerAction: F[ActionCard]

  def askPlayerCounterAction: F[NopeCard]

  def execPlayerAction (c: ActionCard): F[GameState]
}

