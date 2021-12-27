package game.gamestate

import player.domain.Username
import game.domain.Command._
import game.domain.ServerResponse._
import game.domain.Game
import card.domain.{CardPile, Card, Defuse, ExplodingCat}
import card.domain.ActionCard
import error.{NotThePlayersTurn, PlayerNotRegistered, PlayerDoesNotHaveCard}
import cats.ApplicativeError
import cats.effect.Temporal
import scala.concurrent.duration._        
import fs2.Stream
import fs2.concurrent.Signal
import fs2.concurrent.SignallingRef

final case class WaitPlayerAction (game: Game) extends GameState {

  private val Game(players, drawPile, cardDecks) = game

  private val maxWait = 6.seconds

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
          newPlayers
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
          newPlayers
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
        newPlayers
      )
    )
  }

  def onDrawCard [F[_]] (u: Username) (
    implicit ae: ApplicativeError[F,Throwable]
  ): Stream[F, ServerCommand] = {

    val currUsername = players.currentPlayer.username
    if (u != currUsername) GameState.unexpectedError(
      NotThePlayersTurn(currUsername, u)
    )
    else Stream
      .eval(GameState.validatePlayerInMap(u, cardDecks))
      .evalMap(_ => popTopCard)
      .flatMap { case (card, newDrawPile) => 
        if (card == ExplodingCat) onExplodingCat(u, newDrawPile)
        else onRegularCard(u, card, newDrawPile)
      }
  }



  def onPlayCard [F[_]: Temporal] (u: Username, card: Card with ActionCard) (
    implicit ae: ApplicativeError[F,Throwable]
  ): Stream[F, ServerCommand] = 
    Stream.eval(
      GameState.validatePlayerInMap(u, cardDecks)
    ) >> (
      if (cardDecks(u) contains card) {

        Stream(
          Broadcast(Ok(s"Player ${u.name} wants to play ${card.name}")),
          Broadcast(Ok(s"Waiting for a Nope Card (anyone can play it)")),
          UpdateState(WaitNopeCard(game, card)),
          StartCountdown(
            maxWait,
            SendResponse(Ok("Finished!!!"))
          )
        )
      }
      else GameState.unexpectedError(
        PlayerDoesNotHaveCard(card)
      )
    )

  def interpret [F[_]: Temporal] (u: Username, cmd: PlayerCommand) (
    implicit ae: ApplicativeError[F,Throwable]
  ): Stream[F,ServerCommand] = cmd match {
    case DrawCard       => onDrawCard(u)
    case PlayCard(card) => onPlayCard(u, card)
    case cmd            => GameState.unexpectedCommand[F](u, cmd, this)
  }
}