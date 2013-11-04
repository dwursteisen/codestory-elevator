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

    "être extrapolé" in {
      val path: Seq[Operation] = Elevator.extrapolatePath(Seq(Call(0, GoUp)))
      path must be equalTo Seq(Call(0, GoUp, go=Go(Call(0, GoUp), 1)), Go(Call(0, GoUp), 1))
    }

    "avoir des ticks" in {
      val go: Go = Go(Call(0, GoUp), 1)
      val path = Elevator.tickIt(0, 0, Seq(Call(0, GoUp, go = go), go))
      path.find(n => n match {
        case node:Go => true
        case _ => false
      }).get must be equalTo Go(Call(0, GoUp), 1, 2)
    }

    "avoir un changement d'operation" in {
      val path = Elevator.replace(Seq(Go(Call(0, GoUp), 1)), Go(Call(0, GoUp), 1), Go(Call(0,GoUp), 1, 1))
      path must be equalTo Seq(Go(Call(0,GoUp), 1, 1))
    }

    "avoir un score" in {
      val goFrom0To2 = Go(Call(0, GoUp, 0), 2, 10)
      val call0To2 = Call(0, GoUp, 0, goFrom0To2)

      Elevator.scoreIt(0, 0, Seq(call0To2, goFrom0To2)) must be equalTo 14
    }

  }

}
