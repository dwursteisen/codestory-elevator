import org.specs2.mutable.Specification
import services.Elevator
import services.model._

/**
 * Created by david on 27/09/13.
 */
class ElevatorSpec extends Specification {

  "L'ascenceur" should {
    "passer par tous les étages" in {
        val actions = Elevator.toActions(0, Closed, Seq(1, 2))
        actions must be equalTo Seq(Up, Open, Close, Up, Open)
    }

    "passer a l'étage suivant" in {
        val actions = Elevator.toActions(0, Closed, Seq(1))
        actions must be equalTo Seq(Up, Open)
    }

    "s'ouvrir" in {
      val actions = Elevator.toActions(0, Closed, Seq(0))
      actions must be equalTo Seq(Open)
    }

    "rester ouvert" in {
        val actions = Elevator.toActions(0, Opened, Seq(0))
        actions must be equalTo Seq(Nothing)
    }

  }

}
