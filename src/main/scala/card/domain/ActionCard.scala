package card.domain

import io.circe.{Decoder, HCursor}

sealed trait ActionCard {

  // def action: Unit
}

object ActionCard {

  val cardsByName: Map[String, Card with ActionCard] = Map.from(List(
    Nope,
    Skip,
    Favor,
    Shuffle,
    SeeTheFuture,
    Attack,
    Defuse
  ).map { c => c.name -> c })

  implicit val decodeCard = new Decoder[Card with ActionCard] {

    def apply (c: HCursor) = c.get("name") { Decoder[String].emap { card =>
      cardsByName.get(card).toRight(s"Card $card does not exists")
    } }
  }
}

final case object Nope extends Card with ActionCard {

  val name = "Nope"
  val description = "Invalidate the action of another player. Play this card any time."
}

final case object Skip extends Card with ActionCard {

  val name = "Skip"
  val description = "End your turn without drawing a card."
}

final case object Favor extends Card with ActionCard {

  val name = "Favor"
  val description = "Force any player to give you a card of their choosing from their hand."
}

final case object Shuffle extends Card with ActionCard {

  val name = "Shuffle"
  val description = "Randomly shuffle the draw pile."
}

final case object SeeTheFuture extends Card with ActionCard {

  val name = "SeeTheFuture"
  val description = "View the top three cards of the draw pile privately."
}

final case object Attack extends Card with ActionCard {

  val name = "Attack"
  val description = "End your turn without drawing a card. The next player has to take two turns."
}

final case object Defuse extends Card with ActionCard {

  val name = "Defuse"
  val description = "Place the Exploding Cat card anywhere in the draw pile."
}