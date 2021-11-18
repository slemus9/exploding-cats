package game.domain

import card.domain.ActionCard
import card.domain.NopeCard

sealed trait GamePhase

final case class Start (game: GameState) extends GamePhase
final case class End (game: GameState) extends GamePhase
final case class WaitAction (game: GameState) extends GamePhase
final case class ExecAction [C <: ActionCard] (game: GameState, actionCard: C) extends GamePhase
final case class WaitCounterAction (game: GameState) extends GamePhase
