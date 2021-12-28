package gamebuilders

package gamebuilders

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import player.domain.Username
import game.gamebuilders.ExplodingCatsBuilder
import card.domain.Defuse
import card.domain.ExplodingCat

class ExplodingCatsBuilderSpec extends AnyWordSpec 
  with Matchers {

  "ExplodingCatsBuilder" should {

    "have 56 cards and deal 8 cards per player" in {

      val ecb = ExplodingCatsBuilder.newGame(
        ExplodingCatsBuilderSpec.players5
      ).getOrElse(
        sys.error("Expected 'Right'")
      )

      assert(ecb.players.map(_.cards.size).forall(_ == 8))
      ecb.drawPile.size + ecb.players.map(_.cards.size).sum shouldBe 56
    }

    "deal at least 1 Defuse card to each player" in {
      
      val ecb = ExplodingCatsBuilder.newGame(
        ExplodingCatsBuilderSpec.players5
      ).getOrElse(
        sys.error("Expected 'Right'")
      )

      assert(ecb.players.forall(
        _.cards.exists(_.isInstanceOf[Defuse.type])
      ))
    }

    "have a number of Exploding Cats which is 1 fewer than the number of players" in {

      val players5 = ExplodingCatsBuilderSpec.players5
      val allPlayers = List.range(2, players5.size + 1).map(
        size => players5.take(size)
      )

      allPlayers.foreach { players =>

        val ecb = ExplodingCatsBuilder.newGame(
          players
        ).getOrElse(
          sys.error("Expected 'Right'")
        )

        val explodingCats = ecb.drawPile.filter(
          _.isInstanceOf[ExplodingCat.type]
        )

        explodingCats.size shouldBe (players.size - 1)
      }
    }

    "only have 2 extra Defuse cards for 2 or 3 players" in {

      val ecb2 = ExplodingCatsBuilder.newGame(
        ExplodingCatsBuilderSpec.players5.take(2)
      ).getOrElse(
        sys.error("Expected 'Right'")
      )

      val ecb3 = ExplodingCatsBuilder.newGame(
        ExplodingCatsBuilderSpec.players5.take(3)
      ).getOrElse(
        sys.error("Expected 'Right'")
      )

      val ecb2Players = ecb2.players.flatMap(
        _.cards.filter(_.isInstanceOf[Defuse.type])
      )
      val ecb2Draw = ecb2.drawPile.filter(
        _.isInstanceOf[Defuse.type]
      )

      val ecb3Players = ecb3.players.flatMap(
        _.cards.filter(_.isInstanceOf[Defuse.type])
      )
      val ecb3Draw = ecb3.drawPile.filter(
        _.isInstanceOf[Defuse.type]
      )

      ecb2Players.size + ecb2Draw.size shouldBe 4
      ecb3Players.size + ecb3Draw.size shouldBe 5
    }
  }
}

object ExplodingCatsBuilderSpec {

  val players5 = List.range(1, 6).map { i => 
    Username.from(s"player$i")  
  }.collect {
    case Right(u) => u
  }

}