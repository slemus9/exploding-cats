package player

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.EitherValues
import player.domain.Username
import player.domain.Player
import player.domain.PlayerSeq
import error.{NoActivePlayers, ShouldNotBeEmpty}

class PlayerSeqSpec extends AnyWordSpec 
  with Matchers
  with EitherValues {

  "PlayerSeq" should {

    "be cyclic" in {

      val ps = PlayerSeqSpec.playerSeq.getOrElse(
        sys.error("Expected 'Right'")
      )

      val ps5 = ps.moveForward.moveForward.moveForward.moveForward
      val ps8 = ps.moveBackwards.moveBackwards.moveBackwards
      val ps1 = ps8.moveForward.moveForward.moveForward

      ps5.currentPlayer.username.name shouldBe "player5"
      ps8.currentPlayer.username.name shouldBe "player8"
      ps1.currentPlayer.username.name shouldBe "player1"
    }

    "never be empty" in {

      val psLeft = PlayerSeq.from(List.empty)
      val psSingle = PlayerSeq.from(
        PlayerSeqSpec.playerList.take(1)
      ).getOrElse(sys.error("Expected 'Right'"))

      psLeft.left.value shouldBe NoActivePlayers
      psSingle.eliminateCurrentPlayer.left.value shouldBe ShouldNotBeEmpty
    }
  }
}

object PlayerSeqSpec {

  val playerList = List.range(1, 11).map { i => 
    Username.from(s"player$i")  
  }.collect {
    case Right(u) => Player(u)
  }

  val playerSeq = PlayerSeq.from(playerList)
}