package game.domain

import player.domain.PlayerSeq
import card.domain.CardPile
import player.domain.Username
import card.domain.CardDeck
import cats.ApplicativeError
import error.PlayerNotRegistered

final case class Game (
  players: PlayerSeq,
  drawPile: CardPile,
  cardDecks: Map[Username, CardDeck]
) {

  def validatePlayerRegistration [F[_]] (u: Username) (
    implicit ae: ApplicativeError[F, Throwable]
  ) = ae.raiseUnless(cardDecks contains u)(
    PlayerNotRegistered(u)
  )
}