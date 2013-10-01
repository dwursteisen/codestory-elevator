import services.{Elevator, ShortestPathElevator}
import services.model.{GoDown, GoUp}

/**
 * Created by david on 28/09/13.
 */
object Replayer {

  var elevator:Elevator = ShortestPathElevator

  def parseLine(log: String): String = {
    val subLog = log.substring(log.lastIndexOf("/"))
    if (!subLog.startsWith("/nextCommand")) {
      subLog
    } else {
      "/nextCommand"
    }

  }


  def parseLogs(logs: String): Seq[String] = {
    logs.split("\n").map(parseLine)
  }


  def replay(logs: String) = {
    val goPattern = "/go\\?floorToGo=(\\d)".r
    val callPattern = "/call\\?atFloor=(\\d)&to=(.*)".r
    val resetPattern = "/reset\\?cause=(.*)".r
    val parsedLogs = parseLogs(logs)

    elevator.reset("Starting test")
    for (call <- parsedLogs) call match {
      case "/nextCommand" => elevator.nextCommand()
      case goPattern(floor) => elevator.go(Integer.parseInt(floor))
      case callPattern(floor, "UP") => elevator.call(Integer.parseInt(floor), GoUp)
      case callPattern(floor, "DOWN") => elevator.call(Integer.parseInt(floor), GoDown)
      case resetPattern(message) => elevator.reset(message)
      case _ => ()
    }
  }

}
