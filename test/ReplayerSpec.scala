import org.specs2.mutable.Specification
import services.Elevator
import services.model.{Up, Open, Close}

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

    "ne pas rester en NOTHING" in {
      val scenario =
        "2013-09-28 18:04:16.231     HTTPElevator http://localhost:9000/call?atFloor=2&to=DOWN\n" +
          "2013-09-28 18:04:16.282     HTTPElevator http://localhost:9000/nextCommand NOTHING\n" +
          "2013-09-28 18:04:17.224     HTTPElevator http://localhost:9000/call?atFloor=4&to=UP\n" +
          "2013-09-28 18:04:17.249     HTTPElevator http://localhost:9000/nextCommand UP\n" +
          "2013-09-28 18:04:18.224     HTTPElevator http://localhost:9000/call?atFloor=4&to=DOWN\n" +
          "2013-09-28 18:04:18.249     HTTPElevator http://localhost:9000/nextCommand UP\n" +
          "2013-09-28 18:04:19.242     HTTPElevator http://localhost:9000/nextCommand OPEN\n" +
          "2013-09-28 18:04:19.244     HTTPElevator http://localhost:9000/userHasEntered\n" +
          "2013-09-28 18:04:19.246     HTTPElevator http://localhost:9000/go?floorToGo=0\n" +
          "2013-09-28 18:04:20.243     HTTPElevator http://localhost:9000/nextCommand CLOSE\n" +
          "2013-09-28 18:04:21.241     HTTPElevator http://localhost:9000/nextCommand UP\n" +
          "2013-09-28 18:04:22.243     HTTPElevator http://localhost:9000/nextCommand UP\n" +
          "2013-09-28 18:04:23.240     HTTPElevator http://localhost:9000/nextCommand OPEN\n" +
          "2013-09-28 18:04:23.241     HTTPElevator http://localhost:9000/userHasEntered\n" +
          "2013-09-28 18:04:23.243     HTTPElevator http://localhost:9000/go?floorToGo=0\n" +
          "2013-09-28 18:04:23.244     HTTPElevator http://localhost:9000/userHasEntered\n" +
          "2013-09-28 18:04:23.245     HTTPElevator http://localhost:9000/go?floorToGo=5\n" +
          "2013-09-28 18:04:24.240     HTTPElevator http://localhost:9000/nextCommand CLOSE\n" +
          "2013-09-28 18:04:25.240     HTTPElevator http://localhost:9000/nextCommand UP\n" +
          "2013-09-28 18:04:26.240     HTTPElevator http://localhost:9000/nextCommand OPEN\n" +
          "2013-09-28 18:04:26.241     HTTPElevator http://localhost:9000/userHasExited\n" +
          "2013-09-28 18:04:27.224     HTTPElevator http://localhost:9000/call?atFloor=3&to=DOWN\n" +
          "2013-09-28 18:04:27.239     HTTPElevator http://localhost:9000/nextCommand CLOSE\n" +
          "2013-09-28 18:04:28.240     HTTPElevator http://localhost:9000/nextCommand DOWN\n" +
          "2013-09-28 18:04:29.239     HTTPElevator http://localhost:9000/nextCommand DOWN\n" +
          "2013-09-28 18:04:30.242     HTTPElevator http://localhost:9000/nextCommand OPEN\n" +
          "2013-09-28 18:04:30.243     HTTPElevator http://localhost:9000/userHasEntered\n" +
          "2013-09-28 18:04:30.245     HTTPElevator http://localhost:9000/go?floorToGo=0\n" +
          "2013-09-28 18:04:31.238     HTTPElevator http://localhost:9000/nextCommand CLOSE\n" +
          "2013-09-28 18:04:32.238     HTTPElevator http://localhost:9000/nextCommand DOWN\n" +
          "2013-09-28 18:04:33.234     HTTPElevator http://localhost:9000/nextCommand DOWN\n" +
          "2013-09-28 18:04:34.238     HTTPElevator http://localhost:9000/nextCommand DOWN\n" +
          "2013-09-28 18:04:35.240     HTTPElevator http://localhost:9000/nextCommand OPEN\n" +
          "2013-09-28 18:04:35.241     HTTPElevator http://localhost:9000/userHasExited\n" +
          "2013-09-28 18:04:35.242     HTTPElevator http://localhost:9000/userHasExited\n" +
          "2013-09-28 18:04:35.246     HTTPElevator http://localhost:9000/userHasExited\n" +
          "2013-09-28 18:04:36.224     HTTPElevator http://localhost:9000/call?atFloor=0&to=UP\n" +
          "2013-09-28 18:04:36.241     HTTPElevator http://localhost:9000/nextCommand NOTHING\n" +
          "2013-09-28 18:04:37.224     HTTPElevator http://localhost:9000/call?atFloor=0&to=UP\n" +
          "2013-09-28 18:04:37.239     HTTPElevator http://localhost:9000/nextCommand NOTHING\n" +
          "2013-09-28 18:04:38.224     HTTPElevator http://localhost:9000/call?atFloor=0&to=UP\n" +
          "2013-09-28 18:04:38.239     HTTPElevator http://localhost:9000/nextCommand NOTHING\n"
      Replayer.replay(scenario)
      Elevator.nextCommand() must be equalTo Close
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
