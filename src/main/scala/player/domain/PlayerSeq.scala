package player.domain

import cats.collections.Dequeue
import error.{GameError, NoActivePlayers, ShouldNotBeEmpty}
import cats.data.NonEmptyList
import cats.syntax.either._

final case class PlayerSeq private (
  private val players: Dequeue[Player]
) extends AnyVal {

  def size = players.size

  def currentPlayer: Player = 
    players.frontOption.get
  
  def updateCurrentPlayer (update: Player => Player): PlayerSeq =
    players.uncons.map { 
      case (p, ps) => PlayerSeq(update(p) +: ps)
    }.get

  def moveForward: PlayerSeq = players.uncons.map {
    case (p, ps) => PlayerSeq(ps :+ p)
  }.get

  def moveBackwards: PlayerSeq = players.unsnoc.map {
    case (p, ps) => PlayerSeq(p +: ps)
  }.get

  def eliminateCurrentPlayer: Either[GameError, PlayerSeq] = 
    if (size > 1)
      players.uncons
        .map { case (_, dq) => PlayerSeq(dq) }    
        .toRight(NoActivePlayers)
    else ShouldNotBeEmpty.asLeft

}    

object PlayerSeq {

  def from (players: List[Player]) = 
    Either.cond(
      players.size > 0,
      PlayerSeq(Dequeue.fromFoldable(players)),
      NoActivePlayers
    )
}