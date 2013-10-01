import org.specs2.mutable.Specification
import services.ShortestPathElevator
import services.model._

/**
 * Created by david on 27/09/13.
 */
class ElevatorSpec extends Specification {

  "L'ascenceur" should {
    "passer par tous les étages" in {
      val actions = ShortestPathElevator.toActions(0, Closed, Seq(1, 2))
      actions must be equalTo Seq(Up, Open, Close, Up, Open)
    }

    "passer a l'étage suivant" in {
      val actions = ShortestPathElevator.toActions(0, Closed, Seq(1))
      actions must be equalTo Seq(Up, Open)
    }

    "s'ouvrir" in {
      val actions = ShortestPathElevator.toActions(0, Closed, Seq(0))
      actions must be equalTo Seq(Open)
    }

    "rester ouvert" in {
      val actions = ShortestPathElevator.toActions(0, Opened, Seq(0))
      actions must be equalTo Seq(Nothing)
    }

    "gérer un scénario simple" in {
      ShortestPathElevator.reset("")
      ShortestPathElevator.call(1, GoUp)
      ShortestPathElevator.nextCommand() must be equalTo Up
      ShortestPathElevator.nextCommand() must be equalTo Open

    }

  }


  "le chemin " should {
    "avoir un score" in {
      ShortestPathElevator.scoreThisPath(0, Seq(Go(0))) must be equalTo 10
      ShortestPathElevator.scoreThisPath(0, Seq(Go(0), Go(0))) must be equalTo 20
      ShortestPathElevator.scoreThisPath(0, Seq(Go(0), Go(1), Go(0))) must be equalTo 20
      ShortestPathElevator.scoreThisPath(0, Seq(Go(0), Go(0), Go(1))) must be equalTo 25
    }

    "être transformé en roadmap" in {
      val roadMap: Seq[Int] = ShortestPathElevator.pathToRoadMap(Seq(Go(0), Go(1), Go(1), Go(2)))
      roadMap must be equalTo Seq(0, 1, 2)
    }
  }


}
