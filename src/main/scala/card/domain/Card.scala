package card.domain

import io.circe.syntax._
import io.circe.{Json, Encoder, Decoder, HCursor}

trait Card {

  val name: String
  val description: String
}

object Card extends{

  val cardsByName: Map[String, Card] = Map.from(List(
    CatCard1,
    CatCard2,
    CatCard3,
    CatCard4,
    CatCard5,
    ExplodingCat
  ).map { c => c.name -> c }) ++ ActionCard.cardsByName

  implicit def encodeCard [C <: Card] = new Encoder[C] {

    def apply (card: C): Json = Json.obj(
      "name"        -> card.name.asJson,
      "description" -> card.description.asJson
    )
  }

  implicit val decodeCard = new Decoder[Card] {

    def apply (c: HCursor) = c.get("name") { Decoder[String].emap { card =>
      cardsByName.get(card).toRight(s"Card '$card' does not exists")
    } }
  }
}

final case object CatCard1 extends Card {

  val name = "CatCard1"
  val description = "This is CatCard1."
}

final case object CatCard2 extends Card {

  val name = "CatCard2"
  val description = "This is CatCard2."
}

final case object CatCard3 extends Card {

  val name = "CatCard3"
  val description = "This is CatCard3."
}

final case object CatCard4 extends Card {

  val name = "CatCard4"
  val description = "This is CatCard4."
}

final case object CatCard5 extends Card {

  val name = "CatCard5"
  val description = "This is CatCard5."
}

final case object ExplodingCat extends Card {

  val name = "ExplodingCat"
  val description = "You die unless you have a Defuse card."
}

final case object Defuse extends Card {

  val name = "Defuse"
  val description = "Place the Exploding Cat card anywhere in the draw pile."
}

final case object Nope extends Card {

  val name = "Nope"
  val description = "Invalidate the action of another player. Play this card any time."
}