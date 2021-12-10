package player.domain

import cats.collections.Dequeue
import error.{GameError, NoActivePlayers}

final case class PlayerSeq private (
  private val players: Dequeue[Username]
) extends AnyVal {

  def size = players.size

  def currentPlayer: Either[GameError, Username] = 
    players.frontOption.toRight(NoActivePlayers)
  
  def moveForward: Either[GameError, PlayerSeq] = players.uncons.map {
    case (p, ps) => PlayerSeq(ps :+ p)
  }.toRight(NoActivePlayers)

  def moveBackwards: Either[GameError, PlayerSeq] = players.unsnoc.map {
    case (p, ps) => PlayerSeq(p +: ps)
  }.toRight(NoActivePlayers)

  def eliminateCurrentPlayer: Either[GameError, PlayerSeq] = 
    players.uncons
      .map { case (_, dq) => PlayerSeq(dq) }
      .toRight(NoActivePlayers)
}

object PlayerSeq {

  def from (players: List[Username]) = 
    PlayerSeq(Dequeue.fromFoldable(players))
}