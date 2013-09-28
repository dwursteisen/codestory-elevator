import org.specs2.mutable.Specification
import services.Elevator
import services.model.{Up, Open, Nothing, Close}

/**
 * Created by david on 28/09/13.
 */
class ReplayerSpec extends Specification {

  "un scenario" should {
    "être rejouer" in {
      val scenario =
          "2013-09-28 01:10:49.461     HTTPElevator http://localhost:9000/call?atFloor=0&to=UP\n" +
          "2013-09-28 01:10:49.478     HTTPElevator http://localhost:9000/nextCommand OPEN\n" +
          "2013-09-28 01:10:48.482     HTTPElevator http://localhost:9000/userHasEntered\n" +
          "2013-09-28 01:10:48.485     HTTPElevator http://localhost:9000/go?floorToGo=5\n" +
          "2013-09-28 01:10:49.461     HTTPElevator http://localhost:9000/call?atFloor=0&to=UP\n" +
          "2013-09-28 01:10:49.478     HTTPElevator http://localhost:9000/nextCommand NOTHING\n" +
          "2013-09-28 01:10:50.461     HTTPElevator http://localhost:9000/call?atFloor=2&to=DOWN\n"
      Replayer.replay(scenario)
      Elevator.nextCommand() must be equalTo Close
      Elevator.nextCommand() must be equalTo Up
      Elevator.nextCommand() must be equalTo Up
      Elevator.nextCommand() must be equalTo Open
      Elevator.nextCommand() must be equalTo Close
      Elevator.nextCommand() must be equalTo Up
      Elevator.nextCommand() must be equalTo Up
      Elevator.nextCommand() must be equalTo Up
      Elevator.nextCommand() must be equalTo Open
    }
  }

  "un log" should {
    "être parser" in {
      val line: String = Replayer.parseLine("2013-09-28 01:10:34.537     HTTPElevator http://localhost:9000/call?atFloor=0&to=UP")
      line must be equalTo "/call?atFloor=0&to=UP"
    }

    "être parser & supprimer le text de fin" in {
      val line: String = Replayer.parseLine("2013-09-28 01:10:35.491     HTTPElevator http://localhost:9000/nextCommand OPEN")
      line must be equalTo "/nextCommand"
    }
  }


  /*


2013-09-28 01:10:49.461     HTTPElevator http://localhost:9000/call?atFloor=0&to=UP
2013-09-28 01:10:48.482     HTTPElevator http://localhost:9000/userHasEntered
2013-09-28 01:10:48.485     HTTPElevator http://localhost:9000/go?floorToGo=5
2013-09-28 01:10:49.461     HTTPElevator http://localhost:9000/call?atFloor=0&to=UP
2013-09-28 01:10:49.478     HTTPElevator http://localhost:9000/nextCommand NOTHING
2013-09-28 01:10:50.461     HTTPElevator http://localhost:9000/call?atFloor=2&to=DOWN
2013-09-28 01:10:50.481     HTTPElevator http://localhost:9000/nextCommand NOTHING
2013-09-28 01:10:51.494     HTTPElevator http://localhost:9000/nextCommand NOTHING

   */
}
