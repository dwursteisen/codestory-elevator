import play.Logger
import services.{Elevator}
import services.model.{GoDown, GoUp}

/**
 * Created by david on 28/09/13.
 */
class Replayer(val elevator: Elevator = ShortestPathElevator) {

  var line: Int = 0

  def parseLine(log: String): String = {
    val subLog = log.substring(log.lastIndexOf("/"))
    subLog
  }


  def parseLogs(logs: String): Seq[String] = {
    logs.split("\n").map(parseLine)
  }


  def replay(logs: String) = {
    val goPattern = "/go\\?floorToGo=(\\d)".r
    val callPattern = "/call\\?atFloor=(\\d)&to=(.*)".r
    val resetPattern = "/reset\\?cause=(.*)".r
    val nextCommandPattern = "/nextCommand (.*)".r

    val parsedLogs = parseLogs(logs)

    elevator.reset("Starting test", 0, 19, 49)
    for (call <- parsedLogs) {
      line = line + 1
      Logger.error({"Command %s".format(call)})
      call match {
        case nextCommandPattern(expectedCommand) => {
          val cmd = elevator.nextCommand()
          if (cmd.toString != expectedCommand) {
            Logger.error({"---- Error bellow ----"})
            throw new RuntimeException("At line %d -> Expected command : %s - actual %s : ".format(line, expectedCommand, cmd))
          }
        }
        case goPattern(floor) => elevator.go(Integer.parseInt(floor))
        case callPattern(floor, "UP") => elevator.call(Integer.parseInt(floor), GoUp)
        case callPattern(floor, "DOWN") => elevator.call(Integer.parseInt(floor), GoDown)
        case resetPattern(message) => elevator.reset(message, 0, 19, 49)
        case _ => ()
      }
    }

  }

}
