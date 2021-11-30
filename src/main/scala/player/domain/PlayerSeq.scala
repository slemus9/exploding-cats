package player.domain

import cats.collections.Dequeue

sealed abstract case class PlayerSeq private (
  private val players: Dequeue[Player]
) {

  def size = players.size

  def currentPlayer = players.frontOption
  
  def moveForward = players.uncons.map {
    case (p, ps) => PlayerSeq(ps :+ p)
  }

  def moveBackwards = players.unsnoc.map {
    case (p, ps) => PlayerSeq(p +: ps)
  }

  def eliminateCurrentPlayer = players.uncons.map(_._2)
}

object PlayerSeq {

  sealed trait PlayerSeqError
  final case object NotEnoughPlayers extends PlayerSeqError

  private def apply (d: Dequeue[Player]) = new PlayerSeq(d) {}

  def fromList (players: List[Player]): Either[PlayerSeqError, PlayerSeq] = Either.cond(
    players.length > 1,
    PlayerSeq(Dequeue.fromFoldable(players)),
    NotEnoughPlayers
  )
}