package game.domain

import card.domain.ActionCard
import player.domain.Username

sealed trait PlayerCommand

final case class Ready (username: Username) extends PlayerCommand
final case class Nope (card: Nope) extends PlayerCommand

sealed trait PlayerActionCommand extends PlayerCommand
final case object DrawCard extends PlayerActionCommand
final case class PlayCard (card: ActionCard) extends PlayerActionCommand

sealed trait PlayerLifeOrDeathCommand extends PlayerCommand
final case object Explode extends PlayerLifeOrDeathCommand
final case object Defused extends PlayerLifeOrDeathCommand