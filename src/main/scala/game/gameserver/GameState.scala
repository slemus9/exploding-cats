package game.gameserver

import game.gamebuilders.GameBuilder
import game.domain.Game
import player.domain.Username
import error.{GameError, PlayerNotRegistered}
import game.domain.Command._
import game.domain.ServerResponse._
import error.UnexpectedCommand
import fs2.{Stream, Pipe}

import cats.syntax.option._
import player.domain.PlayerSeq
import card.domain.CardPile
import cats.ApplicativeError

sealed trait GameState {

  val reply: String

  def interpret [F[_]] (u: Username, cmd: PlayerCommand) (
    implicit ae: ApplicativeError[F, Throwable]
  ): Stream[F, ServerCommand]
}
object GameState {

  def initialState (builder: GameBuilder): GameState =
    WaitPlayers(builder, none, Map.empty)

  def unexpectedError [F[_]] (
    t: Throwable
  ) = Stream(SendResponse(UnexpectedError(t)))

  def unexpected [F[_]] (
    username: Username, 
    cmd: PlayerCommand, 
    gameState: GameState
  ): Stream[F, ServerCommand] = unexpectedError(
    UnexpectedCommand(gameState, cmd)
  )

  def sendCurrentPlayer [F[_]] (
    players: PlayerSeq
  ) (
    implicit ae: ApplicativeError[F, Throwable]
  ): F[ServerCommand] =
    ae.fromEither(players.currentPlayer.map { u => 
      Broadcast(Ok(s"It's ${u.username}'s turn!"))  
    })
}

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
        SendResponse(Ok(s"Welcome to Exploding Kittens ${u.username}!")),
        UpdateState(updated),
        Broadcast(Ok(s"Player ${u.username} joined the game")),
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
      g => {
        
        val players = PlayerSeq.from(g.players.map(_.username))
        val cardPile = CardPile(g.drawPile)
        Stream(
          Broadcast(Ok("Starting game...")),
          DealCards(g.players)
        ) ++ Stream.eval(
          GameState.sendCurrentPlayer(
            players
          )
        ) ++ Stream(
          UpdateState(WaitPlayerAction(players, cardPile))
        )
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
            Broadcast(Ok(s"Player ${u.username} is ready!")),
            Broadcast(Ok(updated.reply))
          )
        }
    } getOrElse GameState.unexpectedError(
      PlayerNotRegistered(u)
    )

  def interpret [F[_]] (u: Username, cmd: PlayerCommand) (
    implicit ae: ApplicativeError[F, Throwable]
  ): Stream[F, ServerCommand] = 
    cmd match {
      case Connect  => onConnect[F](u)
      case Ready    => onReady[F](u)
      case cmd      => GameState.unexpected[F](u, cmd, this)
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

final case class WaitPlayerAction (players: PlayerSeq, cardPile: CardPile) extends GameState {

  private val currentPlayer = players.currentPlayer

  def onDrawCard [F[_]] (
    implicit ae: ApplicativeError[F,Throwable]
  ): Stream[F, ServerCommand] = Stream.eval(
    ae.fromEither(cardPile.drawTopCard)
  ).flatMap { case (c, newPile) => Stream(
    SendResponse(DrawedCard(c)),
    UpdateState(???)
  )}

  def interpret [F[_]] (u: Username, cmd: PlayerCommand) (
    implicit ae: ApplicativeError[F,Throwable]
  ): Stream[F,ServerCommand] = ???

  val reply: String = ???
}


final case object End extends GameState {

  def interpret [F[_]] (u: Username, cmd: PlayerCommand) (
    implicit ae: ApplicativeError[F,Throwable]
  ): Stream[F,ServerCommand] = ???

  val reply: String = ???  
}