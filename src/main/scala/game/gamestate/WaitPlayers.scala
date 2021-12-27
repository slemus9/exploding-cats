package game.gamestate

import player.domain.{Username, PlayerSeq}
import game.gamebuilders.GameBuilder
import game.domain.Command._
import game.domain.ServerResponse._
import game.domain.Game
import card.domain.{CardPile, CardDeck}
import error.PlayerNotRegistered
import cats.ApplicativeError
import cats.syntax.option._
import cats.effect.Temporal
import fs2.Stream
import player.domain.Player

final case class WaitPlayers (
  builder: GameBuilder, 
  mostRecentPlayer: Option[Username], 
  playersReady: Map[Username, Boolean]
) extends GameState {

  private val maxPlayers = builder.maxNumPlayers
  private val minPlayers = builder.minNumPlayers
  private val currPlayers = playersReady.size
  private val numReady = playersReady.values.count(identity(_))

  private def onConnect [F[_]] (u: Username): Stream[F, ServerCommand] = 
    if (currPlayers == maxPlayers) Stream(
      SendResponse(GameIsFull),
      EndConnection
    )
    else {
      val updated = WaitPlayers(
        builder, 
        u.some, 
        playersReady + (u -> false)
      )

      if (playersReady contains u) Stream(
        SendResponse(Ok("You are already connected"))
      ) else Stream(
        SendResponse(Ok(s"Welcome to Exploding Kittens ${u.name}!")),
        UpdateState(updated),
        Broadcast(Ok(s"Player ${u.name} joined the game")),
        Broadcast(Ok(updated.reply))
      )
    }

  private def setupGame [F[_]] (
    implicit ae: ApplicativeError[F, Throwable]
  ): Stream[F, ServerCommand] = {

    val players = playersReady.keys.toList
    builder.newGame(players).fold(
      t => Stream(SendResponse(
        UnexpectedError(t)
      )),
      { case GameBuilder.GameSetup(playersRaw, cardPile) =>
        
        val players = playersRaw.map { p => Player(p.username) }
        val drawPile = CardPile(cardPile)
        val cardDecks = Map.from(
          playersRaw.map { case GameBuilder.PlayerSetup(u, cards) =>
            u -> CardDeck.from(cards)  
          }
        )

        Stream(
          Broadcast(Ok("Starting game...")),
          DealCards(playersRaw)
        ) ++ Stream.eval(
          ae.fromEither(PlayerSeq.from(players))
        ).flatMap { playerSeq => 
            val game = Game(playerSeq, drawPile, cardDecks)
            Stream(
              SendResponse(CurrentPlayer(playerSeq)),
              UpdateState(WaitPlayerAction(game))
            )
        }
      }
    )
  } 

  private def onReady [F[_]] (u: Username) (
    implicit ae: ApplicativeError[F, Throwable]
  ): Stream[F, ServerCommand] = 
    playersReady.get(u).map {
      case true => Stream(
        SendResponse(Ok("You are ready"))
      )
      case false => 
        if (currPlayers >= minPlayers && numReady + 1 == currPlayers) setupGame
        else {

          val updated = this.copy(
            playersReady = playersReady + (u -> true)
          )

          Stream(
            UpdateState(updated),
            Broadcast(Ok(s"Player ${u.name} is ready!")),
            Broadcast(Ok(updated.reply))
          )
        }
    } getOrElse GameState.unexpectedError(
      PlayerNotRegistered(u)
    )

  def interpret [F[_]: Temporal] (u: Username, cmd: PlayerCommand) (
    implicit ae: ApplicativeError[F, Throwable]
  ): Stream[F, ServerCommand] = 
    cmd match {
      case Connect  => onConnect(u)
      case Ready    => onReady(u)
      case cmd      => GameState.unexpectedCommand(u, cmd, this)
    }

  val reply: String = {
    val readyStr = 
      if (currPlayers >= minPlayers) "There are enough players to play the game."
      else ""
    s"""
      | Waiting Players ...
      | $readyStr
      | max: $maxPlayers, min: $minPlayers,
      | connected: $currPlayers, ready: $numReady""".stripMargin.trim
  }
}