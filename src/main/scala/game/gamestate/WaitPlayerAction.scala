package game.gamestate

import player.domain.Username
import game.domain.Command._
import game.domain.ServerResponse._
import game.domain.Game
import card.domain.{CardPile, Card, Defuse, ExplodingCat}
import error.{NotThePlayersTurn, PlayerNotRegistered}
import cats.ApplicativeError
import fs2.Stream

final case class WaitPlayerAction (game: Game) extends GameState {

  private val Game(players, drawPile, cardDecks) = game

  private def validatePlayerInMap [F[_]] (u: Username) (
    implicit ae: ApplicativeError[F,Throwable]
  ) = ae.raiseUnless(cardDecks contains u)(
    PlayerNotRegistered(u)
  )

  private def popTopCard [F[_]] (
    implicit ae: ApplicativeError[F,Throwable]
  ) = ae.fromEither(
    drawPile.drawTopCard
  )

  private def killCurrentPlayer [F[_]] (
    implicit ae: ApplicativeError[F,Throwable]
  ) = ae.fromEither(
    players.eliminateCurrentPlayer
  )

  private def onExplodingCat [F[_]] (u: Username, newDrawPile: CardPile) (
    implicit ae: ApplicativeError[F,Throwable]
  ) = 
    if (cardDecks(u) contains Defuse) {
      val deck = cardDecks(u) - Defuse
      val newDecks = cardDecks + (u -> deck)
      val newPlayers = players.moveForward
      Stream(
        SendResponse(Ok("You defused the Exploding Cat")),
        SendResponse(PlayerDeck(deck.toList)),
        UpdateState(WaitPlayerAction(Game(
          newPlayers,
          newDrawPile,
          newDecks
        ))),
        GameState.sendCurrentPlayer(
          newPlayers.currentPlayer
        )
      )
    } else Stream.eval(killCurrentPlayer).flatMap { newPlayers =>       
      Stream(
        SendResponse(Ok("You don't have a Defuse card. You exploded!")),
        UpdateState(WaitPlayerAction(game.copy(
          players = newPlayers,
          drawPile = newDrawPile
        ))),
        GameState.sendCurrentPlayer(
          newPlayers.currentPlayer
        )
      )
    }


  private def onRegularCard [F[_]] (u: Username, card: Card, newDrawPile: CardPile) = {
    val deck = cardDecks(u) + card
    val newDecks = cardDecks + (u -> deck)
    val newPlayers = players.moveForward
    Stream(
      SendResponse(NextCardInPile(card)),
      SendResponse(PlayerDeck(deck.toList)),
      UpdateState(WaitPlayerAction(Game(
        newPlayers,
        newDrawPile,
        newDecks
      ))),
      GameState.sendCurrentPlayer(
        newPlayers.currentPlayer
      )
    )
  }

  def onDrawCard [F[_]] (u: Username) (
    implicit ae: ApplicativeError[F,Throwable]
  ): Stream[F, ServerCommand] = 
    if (u != players.currentPlayer) GameState.unexpectedError(
      NotThePlayersTurn(players.currentPlayer, u)
    )
    else Stream
      .eval(validatePlayerInMap(u))
      .evalMap(_ => popTopCard)
      .flatMap { case (card, newDrawPile) => 
        if (card == ExplodingCat) onExplodingCat(u, newDrawPile)
        else onRegularCard(u, card, newDrawPile)
      }


  def interpret [F[_]] (u: Username, cmd: PlayerCommand) (
    implicit ae: ApplicativeError[F,Throwable]
  ): Stream[F,ServerCommand] = cmd match {
    case DrawCard => onDrawCard(u)
    case cmd      => GameState.unexpectedCommand[F](u, cmd, this)
  }
}