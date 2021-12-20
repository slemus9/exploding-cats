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

sealed trait GameState {

  val reply: String

  def interpret [F[_]] (u: Username): Pipe[F, PlayerCommand, ServerCommand]
}
object GameState {

  def initialState (builder: GameBuilder): GameState =
    WaitPlayers(builder, none, Map.empty)

  def unexpected [F[_]] (
    username: Username, 
    cmd: PlayerCommand, 
    gameState: GameState
  ): Stream[F, ServerCommand] = Stream(
    SendResponse(
      UnexpectedError(UnexpectedCommand(gameState, cmd))
    )
  )
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
      Stream(
        SendResponse(Ok(s"Welcome to Exploding Kittens ${u.username}!")),
        UpdateState(updated),
        Broadcast(Ok(s"Player ${u.username} joined the game"))
      )
    }

  // private def setupGame: GameStateUpdate = {

  //   val players = playersReady.keys.toList
  //   builder.newGame(players).fold(
  //     t => GameStateUpdate(End, UnexpectedError(t), none),
  //     g => GameStateUpdate(SetupGame(g), Ok(), DealCards.some) 
  //   )
  // }

  private def onReady [F[_]] (u: Username): Stream[F, ServerCommand] = {
    playersReady.get(u).map {
      case true => Stream(
        SendResponse(Ok("You are ready"))
      )
      case false => 
        if (numReady + 1 == currPlayers) ???
    }
    ???
  }

  // private def onReady (ready: Ready): GameStateUpdate = {

  //   val username = ready.username
  //   playersReady.get(username).map {
  //     case true   => GameStateUpdate(this, Ok("You are ready"), none) 
  //     case false  => 
  //       if (numReady + 1 == currPlayers) setupGame
  //       else {

  //         val updated = this.copy(playersReady = playersReady + (username -> true))
  //         GameStateUpdate(updated, Ok(), None)
  //       } 
  //   } getOrElse GameStateUpdate(
  //     this,
  //     UnexpectedError(PlayerNotRegistered(username)),
  //     none
  //   )
  // }

  def interpret [F[_]] (u: Username): Pipe[F, PlayerCommand, ServerCommand] = 
    _.flatMap {
      case Connect  => onConnect[F](u)
      case Ready    => onReady[F](u)
      case cmd      => GameState.unexpected[F](u, cmd, this)
    }

  // def interpret (cmd: Command): GameStateUpdate = cmd match {
  //   case c: Connect => onConnect(c)
  //   case r: Ready   => onReady(r)
  //   case cmd        => GameStateUpdate(
  //     this,
  //     UnexpectedError(UnexpectedCommand(this, cmd)),
  //     none
  //   )
  // }

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

final case class SetupGame (game: Game) extends GameState {

  def interpret [F[_]] (u: Username): Pipe[F, PlayerCommand, ServerCommand] = ???

  val reply: String = ???
}

final case object End extends GameState {

  def interpret [F[_]] (u: Username): Pipe[F, PlayerCommand, ServerCommand] = ???

  val reply: String = ???  
}