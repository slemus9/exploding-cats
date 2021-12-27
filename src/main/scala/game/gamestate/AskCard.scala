package game.gamestate

import player.domain.Username
import game.domain.Command._
import game.domain.ServerResponse._
import cats.ApplicativeError
import cats.effect.Temporal
import fs2.Stream
import game.domain.Game
import error.UnexpectedPlayer
import error.PlayerDoesNotHaveCard
import card.domain.Card

final case class AskCard (
  sender: Username,
  recipient: Username,
  game: Game
) extends GameState {

  private val Game(players, cardPile, cardDecks) = game

  private def onSendCard [F[_]] (received: Username, card: Card) = {
    if (recipient == received) {
      if (cardDecks(recipient) contains card) {
        val newSenderDeck = cardDecks(sender) + card
        val newRecipientDeck = cardDecks(recipient) - card
        val newDecks = cardDecks + 
          (sender -> newSenderDeck) + 
          (recipient -> newRecipientDeck)

        Stream(
          SendResponse(SendCards(newRecipientDeck.toList)),
          SendResponseTo(sender, Ok(s"Player ${recipient.name} sent you a ${card.name} card")),
          SendResponseTo(sender, SendCards(newSenderDeck.toList)),
          UpdateState(WaitPlayerAction(Game(
            players,
            cardPile,
            newDecks
          )))
        )
      } else GameState.unexpectedError(
        PlayerDoesNotHaveCard(card)
      )


    } else GameState.unexpectedError(
      UnexpectedPlayer(recipient, received)
    )
  }

  def interpret [F[_]: Temporal] (u: Username, cmd: PlayerCommand) (
    implicit ae: ApplicativeError[F,Throwable]
  ): Stream[F, ServerCommand] = {

    val resolve = cmd match {
      case SendCard(card) => onSendCard(u, card)
      case cmd            => GameState.unexpectedCommand(u, cmd, this)
    }

    Stream.eval(
      game.validatePlayerRegistration(u)
    ).evalMap { _ => 
      game.validatePlayerRegistration(sender)
    }.evalMap { _ => 
      game.validatePlayerRegistration(recipient)  
    } >> resolve
  }
}