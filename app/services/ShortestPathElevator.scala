package services

import services.model._
import play.api.Logger

/**
 * Created by david on 26/09/13.
 */
object ShortestPathElevator extends Elevator{

  def toActions(currentFloor: Int, currentStatus: Status, roadmap: Seq[Int]): Seq[Action] = {
    roadmap match {
      case Nil => Nil
      case head :: tail if head equals currentFloor => currentStatus match {
        case Closed => Open +: toActions(currentFloor, Opened, tail)
        case Opened => Nothing +: toActions(currentFloor, Opened, tail)
      }
      case head :: tail if currentFloor < head => currentStatus match {
        case Closed => Up +: toActions(currentFloor + 1, Closed, roadmap)
        case Opened => Close +: toActions(currentFloor, Closed, roadmap)
      }
      case head :: tail if currentFloor > head => currentStatus match {
        case Closed => Down +: toActions(currentFloor - 1, Closed, roadmap)
        case Opened => Close +: toActions(currentFloor, Closed, roadmap)
      }
    }
  }

  var currentFloor = 0
  var currentDirection: Direction = GoUp
  var slowPath = Seq[Operation]()
  var currentStatus: Status = Closed
  var shouldClose = false


  def nextCommand(): Action = {
    val bestPath = shortestPath(currentFloor, currentDirection, slowPath)
    val actions = toActions(currentFloor, currentStatus, pathToRoadMap(bestPath))
    actions match {
      case Nil => ()
      case Open :: tail  => {
        slowPath = slowPath.filterNot(_.isSameFloor(currentFloor))
        currentStatus = Opened
      }
      case Nothing :: tail => {
        slowPath = slowPath.filterNot(_.isSameFloor(currentFloor))
        currentStatus = Opened
      }
      case Close :: tail  => {
        currentStatus = Closed
      }
      case Up :: tail  => {
        currentFloor = Math.min(currentFloor + 1, MAX_FLOOR)
      }
      case Down :: tail  => {
        currentFloor = Math.max(currentFloor -1, MIN_FLOOR)
      }
    }
    // shoudl
    val actionToReturn = actions.headOption.getOrElse(Nothing)
    val result = actionToReturn match {
      case Nothing if shouldClose => {
        shouldClose = false
        currentStatus = Closed
        Close
      }
      case Nothing if !shouldClose => {
        shouldClose = true;
        Nothing
      }
      case action => action
    }

    Logger.info({"NEXT COMMAND %s".format(result)})
    result

  }

  def shortestPath(currentFloor: Int, currentDirection: Direction, roadmap: Seq[Operation] = Seq()): Seq[Operation] = roadmap match {
    case Nil => Seq()
    case _ => {
      val generatedPath = roadmap.map(node => {
          node +: shortestPath(node.floor, currentDirection, roadmap.diff(Seq(node)))
      })
      generatedPath.maxBy(p => scoreThisPath(currentFloor, p))
    }
  }

  def pathToRoadMap(path: Seq[Operation]): Seq[Int] = path match {
    case Nil => Seq()
    case head :: Nil => Seq(head.floor)
    case head :: tail => head.floor +: pathToRoadMap(tail.filterNot(_.isSameFloor(head)))
  }

  def scoreThisPath(currentFloor: Int, path: Seq[Operation]): Double = path match {
    case Nil => 0.0
    case Call(floor, GoUp) :: tail if floor == currentFloor => 5 + scoreThisPath(currentFloor+1, tail)
    case Call(floor, GoDown) :: tail if floor == currentFloor => 5  + scoreThisPath(currentFloor-1, tail)
    case Go(floor) :: tail if floor == currentFloor => 10 + scoreThisPath(currentFloor, tail)
    case head :: tail if head.floor > currentFloor => -5 + scoreThisPath(currentFloor +1, path)
    case head :: tail if head.floor < currentFloor => -5 + scoreThisPath(currentFloor -1, path)

  }


  def call(floor: Int, direction: Direction) = {
    slowPath = slowPath :+ Call(floor, direction)
    shouldClose = false
    Logger.info({"CALL %d %s".format(floor, direction)})
  }

  def go(floor: Int) = {
    slowPath = slowPath :+ Go(floor)
    shouldClose = false
    Logger.info({"GO %d".format(floor)})
  }

  def reset(cause: String, minFloor: Int, maxFloor: Int, maxPassenger: Int) = {
    currentFloor = 0
    currentDirection = GoUp
    slowPath = Seq[Operation]()
    currentStatus = Closed
    shouldClose = false
    Logger.error({"RESET %s".format(cause)})
  }


}
