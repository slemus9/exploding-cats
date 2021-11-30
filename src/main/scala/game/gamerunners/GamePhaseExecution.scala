package game.gamerunners

import game.domain.GameState
import card.domain.ActionCard
import card.domain.Nope

trait GamePhaseExecution [F[_]] {

  def start (gameState: GameState): F[GameState]

  def end (fs: F[GameState]): F[GameState]

  def waitAction (fs: F[GameState]): F[ActionCard]

  def execAction (action: ActionCard) (fs: F[GameState]): F[GameState]

  def waitCounterAction (fs: F[GameState]): F[Nope.type]

  def execCounterAction (nope: Nope.type) (fs: F[GameState]): F[GameState]
}