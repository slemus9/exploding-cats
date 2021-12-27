package card.domain

import cats.data.NonEmptyList

final case class CardDeck private (
  private val cards: Map[Card, Int]
) {

  def + (c: Card) = CardDeck(
    cards + cards.get(c)
      .map { cnt => c -> (cnt + 1) }
      .getOrElse(c -> 1)
  )

  def - (c: Card) = CardDeck(
    cards.get(c)
      .map { cnt => 
        if (cnt == 1) cards - c
        else cards + (c -> (cnt - 1))
      }
      .getOrElse(
        cards
      )
  )

  def contains (c: Card) = 
    cards contains c

  def toList = 
    cards.flatMap { case (c, cnt) => 
      List.fill(cnt)(c) 
    }.toList
  
}

object CardDeck {

  def from (cards: List[Card]) = CardDeck(Map.from(
    cards
      .groupBy(identity(_))
      .map { case (c, group) =>
        c -> group.size 
      }
  ))
}