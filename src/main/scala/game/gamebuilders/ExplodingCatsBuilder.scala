package game.gamebuilders

import card.domain.Card

import game.domain.GameState
import player.domain.Username
import player.domain.Player
import card.domain.CatCard1
import card.domain.CatCard2
import card.domain.CatCard3
import card.domain.CatCard4
import card.domain.CatCard5
import card.domain.Nope
import card.domain.Skip
import card.domain.Favor
import card.domain.Shuffle
import card.domain.SeeTheFuture
import card.domain.Attack
import card.domain.Defuse
import card.domain.ExplodingCat
import scala.util.Random
import cats.data.NonEmptyList

final object ExplodingCatsBuilder extends GameBuilder {


  private val minNumPlayers = 2
  private val maxNumPlayers = 5
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

  private def dealCards (userNames: List[Username]) = {

    val n = userNames.length
    val extraDefuse = 
      if (n == 2 || n == 3) 2
      else numDefuseCards - n

    val cards = Random.shuffle(
      cardsToDeal ++ List.fill(extraDefuse)(Defuse)
    ) 
    
    val (toDeal, remaining) = cards.splitAt(n * initialCardsPerPlayer)
    val decks = toDeal.grouped(n)
    val players = decks.zip(userNames).map {
      case (deck, username) => Player(username, Defuse :: deck)
    }

    GameState(
      players.toList,
      remaining ++ List.fill(n - 1)(ExplodingCat),
      List.empty
    )
  }

  def newGame(userNames: List[Username]): Either[GameBuildingError, GameState] = {
    
    val n = userNames.length
    Either.cond(
      n > minNumPlayers && n < maxNumPlayers,
      dealCards(userNames),
      PlayersOutOfBounds
    )
  }
}