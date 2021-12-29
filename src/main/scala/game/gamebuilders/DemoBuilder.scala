package game.gamebuilders

import error.GameError
import player.domain.Username
import card.domain._
import error.PlayersOutOfBounds

final object DemoBuilder extends GameBuilder {

  val minNumPlayers: Int = 2

  val maxNumPlayers: Int = 2

  def newGame(usernames: List[Username]): Either[GameError, GameBuilder.GameSetup] = {

    val n = usernames.size
    def playerCards () = List(
      Skip, Favor, Shuffle, SeeTheFuture, Attack, Nope, Defuse
    )

    val drawPile = List(
      CatCard1, Nope, ExplodingCat, CatCard2, 
      CatCard3, CatCard5, Nope, CatCard4
    )

    val players = usernames.map { u => 
      GameBuilder.PlayerSetup(u, playerCards())
    }

    Either.cond(
      n >= minNumPlayers && n <= maxNumPlayers,
      GameBuilder.GameSetup(players, drawPile),
      PlayersOutOfBounds(minNumPlayers, maxNumPlayers)
    )
  }


  
}