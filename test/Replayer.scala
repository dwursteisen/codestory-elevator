import services.Elevator
import services.model.{GoDown, GoUp}

/**
 * Created by david on 28/09/13.
 */
object Replayer {
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

    Elevator.reset("Starting test")
    for (call <- parsedLogs) call match {
      case "/nextCommand" => Elevator.nextCommand()
      case goPattern(floor) => Elevator.go(Integer.parseInt(floor))
      case callPattern(floor, "UP") => Elevator.call(Integer.parseInt(floor), GoUp)
      case callPattern(floor, "DOWN") => Elevator.call(Integer.parseInt(floor), GoDown)
      case resetPattern(message) => Elevator.reset(message)
      case _ => ()
    }
  }

}
