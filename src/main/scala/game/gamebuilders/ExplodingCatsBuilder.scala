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
    val decks = toDeal.grouped(n)
    val players = decks.zip(usernames).map {
      case (deck, username) => Player(username, Defuse :: deck)
    }

    Game(
      players.toList,
      remaining ++ List.fill(n - 1)(ExplodingCat)
    )
  }

  def newGame(usernames: List[Username]): Either[GameError, Game] = {
    
    val n = usernames.length
    Either.cond(
      n >= minNumPlayers && n <= maxNumPlayers,
      dealCards(usernames),
      PlayersOutOfBounds(minNumPlayers, maxNumPlayers)
    )
  }
}

// object BuilderTest extends App {

//   import io.circe.syntax._

//   val users = List(
//     Username.from("user1"),
//     Username.from("user2"),
//     Username.from("user3"),
//     Username.from("user4")
//   ).sequence

//   val game = users.flatMap(ExplodingCatsBuilder.newGame)
//   val userJsons = game.map(_.players.map(_.asJson))

//   userJsons.foreach(_.foreach(println))
//   userJsons.foreach(_.foreach(u => println(u.as[Player])))
// }