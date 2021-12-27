import io.circe.syntax._
import io.circe.parser._
import io.circe._
import game.domain.Command._

object TestApp extends App {
  
  val strCmd = 
    """
      |{
      |   "card" : { "name" : "Favor" } 
      |}""".stripMargin.trim

  val jsonCmd = parse(strCmd).getOrElse(Json.Null)

  println(jsonCmd.as[PlayerCommand])
}