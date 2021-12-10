package card.domain

import scala.util.Random
import error.{GameError, CardPileEmpty, InvalidCardPileIndex}
import cats.syntax.either._
import cats.syntax.functor._

final case class CardPile private ( 
  private val cards: List[Card]
) {

  val size = cards.size

  def drawTopCard: Either[GameError, (Card, CardPile)] = cards match {
    case Nil      => CardPileEmpty.asLeft
    case x :: xs  => (x -> CardPile(xs)).asRight
  }

  def shuffle: CardPile = CardPile(
    Random.shuffle(cards)
  )

  private def insertAt (index: Int) = { card: ExplodingCat.type => 
    val (front, back) = cards.splitAt(index)
    CardPile(front ++ (card :: back))
  }

  private def validateIndex (index: Int) =
    if (index < 0 || index >= size) InvalidCardPileIndex.asLeft
    else ().asRight

  def insertExplodingCat (index: Int): ExplodingCat.type => Either[GameError, CardPile] = {
    card => validateIndex(index) as insertAt(index)(card)
  }

  def insertExplodingCatRandomly = insertAt(Random.nextInt(size))
}
