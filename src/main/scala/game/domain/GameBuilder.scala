package game.domain

import player.domain.PlayerSeq

trait GameBuilder [G <: GameState] {

  val minPlayers: Int
  val maxPlayers: Int
  
  // TODO: change for an error type
  def newGame (players: PlayerSeq): Either[String, G]
}

object GameBuilder {
  
  def apply [G <: GameState] (implicit instance: GameBuilder[G]) = instance
}

