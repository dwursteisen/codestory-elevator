import org.specs2.mutable.Specification
import services.ShortestPathElevator
import ShortestPathElevator.Node
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

   /*
  "la cabine " should {
    "aller a l'etage le plus proche " in {
      val roadmap = Elevator.shortestPath(0, Seq(Node(1), Node(2)))
      roadmap must be equalTo Seq(1, 2)
    }
  }
  */

  "le chemin " should {
    "avoir un score" in {
      ShortestPathElevator.scoreThisPath(0, Seq(Node(0))) must be equalTo 10
      ShortestPathElevator.scoreThisPath(0, Seq(Node(0), Node(0))) must be equalTo 20
      ShortestPathElevator.scoreThisPath(0, Seq(Node(0), Node(1), Node(0))) must be equalTo 20
      ShortestPathElevator.scoreThisPath(0, Seq(Node(0), Node(0), Node(1))) must be equalTo 25
    }
  }

  "le chemin " should {
    "être transformé en roadmap" in {
      val roadMap: Seq[Int] = ShortestPathElevator.pathToRoadMap(Seq(Node(0), Node(1), Node(1), Node(2)))
      roadMap must be equalTo Seq(0, 1, 2)
    }
  }


}
