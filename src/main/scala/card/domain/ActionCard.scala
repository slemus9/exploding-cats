package card.domain

sealed trait ActionCard {

  // val instructions: String

  // def action: Unit
}

final case object Nope extends Card with ActionCard

final case object Skip extends Card with ActionCard

final case object Favor extends Card with ActionCard

final case object Shuffle extends Card with ActionCard

final case object SeeTheFuture extends Card with ActionCard

final case object Attack extends Card with ActionCard

final case object Defuse extends Card with ActionCard