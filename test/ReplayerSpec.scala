import org.specs2.mutable.Specification
import services.{SimpleElevator, ShortestPathElevator}
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
      new Replayer(ShortestPathElevator).replay(scenario)
      ShortestPathElevator.nextCommand() must be equalTo Close
      ShortestPathElevator.nextCommand() must be equalTo Up
      ShortestPathElevator.nextCommand() must be equalTo Up
      ShortestPathElevator.nextCommand() must be equalTo Open
      ShortestPathElevator.nextCommand() must be equalTo Close
      ShortestPathElevator.nextCommand() must be equalTo Up
      ShortestPathElevator.nextCommand() must be equalTo Up
      ShortestPathElevator.nextCommand() must be equalTo Up
      ShortestPathElevator.nextCommand() must be equalTo Open
    }

    "doit forcer la fermeture de l'acenceur" in {
      val scenario =
        "2013-09-28 18:04:38.224     HTTPElevator http://localhost:9000/call?atFloor=0&to=UP\n" +
          "2013-09-28 18:04:38.239     HTTPElevator http://localhost:9000/nextCommand OPEN\n" +
          "2013-09-28 18:04:38.224     HTTPElevator http://localhost:9000/call?atFloor=0&to=UP\n" +
          "2013-09-28 18:04:38.239     HTTPElevator http://localhost:9000/nextCommand CLOSE\n"
      new Replayer(SimpleElevator).replay(scenario)
    }

    "ne pas rester en nothing avec SimpleElevator" in {
      val scenario =
        "2013-10-01 14:57:58.013     HTTPElevator http://localhost:9000/v2/reset?cause=the+elevator+is+at+the+lowest+level+and+its+doors+are+closed\n" +
          "2013-10-01 14:57:58.226     HTTPElevator http://localhost:9000/v2/call?atFloor=3&to=UP\n" +
          "2013-10-01 14:57:59.227     HTTPElevator http://localhost:9000/v2/nextCommand UP\n" +
          "2013-10-01 14:58:00.224     HTTPElevator http://localhost:9000/v2/nextCommand UP\n" +
          "2013-10-01 14:57:59.221     HTTPElevator http://localhost:9000/v2/call?atFloor=0&to=UP\n" +
          "2013-10-01 14:58:00.220     HTTPElevator http://localhost:9000/v2/call?atFloor=0&to=UP\n" +
          "2013-10-01 14:58:01.225     HTTPElevator http://localhost:9000/v2/nextCommand UP\n" +
          "2013-10-01 14:58:02.225     HTTPElevator http://localhost:9000/v2/nextCommand OPEN\n" +
          "2013-10-01 14:58:02.226     HTTPElevator http://localhost:9000/v2/userHasEntered\n" +
          "2013-10-01 14:58:02.227     HTTPElevator http://localhost:9000/v2/go?floorToGo=5\n" +
          "2013-10-01 14:58:03.225     HTTPElevator http://localhost:9000/v2/nextCommand CLOSE\n" +
          "2013-10-01 14:58:04.223     HTTPElevator http://localhost:9000/v2/nextCommand UP\n" +
          "2013-10-01 14:58:05.225     HTTPElevator http://localhost:9000/v2/nextCommand UP\n" +
          "2013-10-01 14:58:06.225     HTTPElevator http://localhost:9000/v2/nextCommand OPEN\n" +
          "2013-10-01 14:58:06.226     HTTPElevator http://localhost:9000/v2/userHasExited\n" +
          "2013-10-01 14:58:07.221     HTTPElevator http://localhost:9000/v2/call?atFloor=0&to=UP\n" +
      "2013-10-01 14:58:06.225     HTTPElevator http://localhost:9000/v2/nextCommand CLOSE\n"
        new Replayer(SimpleElevator).replay(scenario)

    }

  }

  "un log" should {
    "être parser" in {
      val line: String = new Replayer().parseLine("2013-09-28 01:10:34.537     HTTPElevator http://localhost:9000/call?atFloor=0&to=UP")
      line must be equalTo "/call?atFloor=0&to=UP"
    }

    "être parser & supprimer le text de fin" in {
      val line: String = new Replayer().parseLine("2013-09-28 01:10:35.491     HTTPElevator http://localhost:9000/nextCommand OPEN")
      line must be equalTo "/nextCommand OPEN"
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
