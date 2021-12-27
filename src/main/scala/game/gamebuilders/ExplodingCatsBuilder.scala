package game.gamebuilders

import scala.util.Random

import game.domain.Game
import player.domain.{Username, Player, PlayerSeq}
import card.domain.{
  Card, CatCard1, CatCard2, CatCard3, CatCard4, CatCard5,
  Nope, Skip, Favor, Shuffle, SeeTheFuture, Attack,
  Defuse, ExplodingCat
}

import cats.syntax.all._
import error.{GameError, PlayersOutOfBounds}
import cats.data.NonEmptyList
import error.NoActivePlayers

final object ExplodingCatsBuilder extends GameBuilder {

  val minNumPlayers = 2
  val maxNumPlayers = 5
  
  private val initialCardsPerPlayer = 7

  private val numDefuseCards = 6

  private def catCardsPerType: (=> Card) => List[Card] = 
    List.fill(4)
  private def nopeCards = List.fill(5)(Nope)
  private def skipCards = List.fill(4)(Skip)
  private def favorCards = List.fill(4)(Favor)
  private def shuffleCards = List.fill(4)(Shuffle)
  private def seeTheFutureCards = List.fill(5)(SeeTheFuture)
  private def attackCards = List.fill(4)(Attack)


  private val cardsToDeal = List.concat(
    catCardsPerType(CatCard1),
    catCardsPerType(CatCard2),
    catCardsPerType(CatCard3),
    catCardsPerType(CatCard4),
    catCardsPerType(CatCard5),
    nopeCards,
    skipCards,
    favorCards,
    shuffleCards,
    seeTheFutureCards,
    attackCards
  )

  private def dealCards (usernames: List[Username]) = {

    val n = usernames.length
    val extraDefuse = 
      if (n == 2 || n == 3) 2
      else numDefuseCards - n

    val cards = Random.shuffle(
      cardsToDeal ++ List.fill(extraDefuse)(Defuse)
    ) 
    
    val (toDeal, remaining) = cards.splitAt(n * initialCardsPerPlayer)
    val decks = toDeal.grouped(initialCardsPerPlayer)
    val players = decks.zip(usernames).map {
      case (deck, username) => GameBuilder.PlayerSetup(
        username, Defuse :: deck 
      )
    }

    GameBuilder.GameSetup(
      players.toList,
      Random.shuffle(
        remaining ++ List.fill(n - 1)(ExplodingCat)
      )
    )
  }

  def newGame(usernames: List[Username]): Either[GameError, GameBuilder.GameSetup] = {
    
    val n = usernames.length
    Either.cond(
      n >= minNumPlayers && n <= maxNumPlayers,
      dealCards(usernames),
      PlayersOutOfBounds(minNumPlayers, maxNumPlayers)
    )
  }
}
