package game.gamebuilders

sealed trait GameBuildingError

final case object PlayersOutOfBounds extends GameBuildingError