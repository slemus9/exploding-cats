package game.domain

import player.domain.Username
import card.domain.ActionCard

final case class CardPlay (
  username: Username,
  actionCard: ActionCard
)