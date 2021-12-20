package game.gameserver

import game.domain.ServerResponse
import game.domain.Command.ServerCommand

final case class GameStateUpdate (
  updated: GameState,
  response: ServerResponse,
  serverCommnand: Option[ServerCommand]
)