package player.domain

import cats.collections.Dequeue
import error.{GameError, NoActivePlayers}
import cats.data.NonEmptyList

final case class PlayerSeq private (
  private val players: Dequeue[Username]
) extends AnyVal {

  def size = players.size

  def currentPlayer: Username = 
    players.frontOption.get
  
  def moveForward: PlayerSeq = players.uncons.map {
    case (p, ps) => PlayerSeq(ps :+ p)
  }.get

  def moveBackwards: PlayerSeq = players.unsnoc.map {
    case (p, ps) => PlayerSeq(p +: ps)
  }.get

  def eliminateCurrentPlayer: Either[GameError, PlayerSeq] = 
    players.uncons
      .map { case (_, dq) => PlayerSeq(dq) }    
      .toRight(NoActivePlayers)
}    

object PlayerSeq {

  def from (players: List[Username]) = 
    Either.cond(
      players.size > 0,
      PlayerSeq(Dequeue.fromFoldable(players)),
      NoActivePlayers
    )
}