package game.domain

import player.domain.PlayerSeq
import card.domain.CardPile

final case class GameState (
  activePlayers: PlayerSeq,
  drawPile: CardPile
)

object GameState {

  def fromGameSetup (game: Game) = GameState(
    PlayerSeq.from(game.players.map(_.username)),
    CardPile(game.drawPile)
  )
}