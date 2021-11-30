package game.domain

import card.domain.ActionCard
import card.domain.Nope

sealed trait GamePhase

final case class Start (gameState: GameState) extends GamePhase
final case class End (gameState: GameState) extends GamePhase
final case class WaitPlayerAction (gameState: GameState) extends GamePhase
final case class ExecPlayerAction (gameState: GameState, c: ActionCard) extends GamePhase
final case class WaitPlayerCounterAction (gameState: GameState) extends GamePhase
final case class ExecPlayerCounterAction (gameState: GameState, nope: Nope.type) extends GamePhase