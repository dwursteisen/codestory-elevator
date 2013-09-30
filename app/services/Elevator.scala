package services

import services.model._

/**
 * Created by david on 26/09/13.
 */
object Elevator {

  case class Node(floor: Int, direction: Option[Direction] = None, callTick: Int = 0) {
    def isSameFloor(other: Node): Boolean = other != null && isSameFloor(other.floor)
    def isSameFloor(floor: Int): Boolean = (floor equals this.floor)
  }

  case class ElevatorStatus(tick: Int = 0,
                    currentFloor: Int = 0,
                    currentDirection: Direction = GoUp,
                    currentStatus: Status = Closed,
                    path: Seq[Node] = Seq(),
                    shouldClose:Boolean = false)

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

  var status = ElevatorStatus()
  val MIN_FLOOR = 0
  val MAX_FLOOR = 6

  def nextCommand(): Action = {
    val bestPath = shortestPath(status)
    val currentFloor = status.currentFloor
    val currentStatus = status.currentStatus
    val actions = toActions(currentFloor, currentStatus, pathToRoadMap(bestPath))
    actions match {
      case Nil => ()
      case Open :: tail  => {
        status = status.copy(currentStatus = Opened, path = status.path.filterNot(_.isSameFloor(currentFloor)))
      }
      case Nothing :: tail => {
        status = status.copy(currentStatus = Opened, path = status.path.filterNot(_.isSameFloor(currentFloor)))
      }
      case Close :: tail  => {
        status = status.copy(currentStatus = Closed)
      }
      case Up :: tail  => {
        status = status.copy(currentFloor = Math.min(currentFloor + 1, MAX_FLOOR))
      }
      case Down :: tail  => {
        status = status.copy(currentFloor = Math.max(currentFloor - 1, MIN_FLOOR))
      }
    }

    status = status.copy(tick = status.tick + 1)

    // shoudl
    val actionToReturn = actions.headOption.getOrElse(Nothing)
    actionToReturn match {
      case Nothing if status.shouldClose => {
        status = status.copy(shouldClose = false, currentStatus = Closed)
        Close
      }
      case Nothing if !status.shouldClose => {
        status = status.copy(shouldClose = true)
        Nothing
      }
      case action => action
    }

  }

  def shortestPath(status: ElevatorStatus): Seq[Node] = status.path match {
    case Nil => Seq()
    case _ => {
      val generatedPath = status.path.map(node => {
          node +: shortestPath(status.copy(currentFloor = node.floor, path=status.path.diff(Seq(node))))
      })
      generatedPath.maxBy(p => scoreThisPath(status.currentFloor, p))
    }
  }

  def pathToRoadMap(path: Seq[Node]): Seq[Int] = path match {
    case Nil => Seq()
    case head :: Nil => Seq(head.floor)
    case head :: tail => head.floor +: pathToRoadMap(tail.filterNot(_.isSameFloor(head)))
  }

  def scoreThisPath(currentFloor: Int, path: Seq[Node]): Double = path match {
    case Nil => 0.0
    case Node(floor, Some(GoUp), _) :: tail if floor == currentFloor => 5 + scoreThisPath(currentFloor+1, tail)
    case Node(floor, Some(GoDown), _) :: tail if floor == currentFloor => 5  + scoreThisPath(currentFloor-1, tail)
    case Node(floor, None, _) :: tail if floor == currentFloor => 10 + scoreThisPath(currentFloor, tail)
    case head :: tail if head.floor > currentFloor => -5 + scoreThisPath(currentFloor +1, path)
    case head :: tail if head.floor < currentFloor => -5 + scoreThisPath(currentFloor -1, path)
  }


  def extrapolatePath(path: Seq[Node]): Seq[Node] = path match {
    case Nil => Nil
    case Node(floor, Some(GoUp), tick)::tail => Node(Math.min(floor + 1, MAX_FLOOR), callTick = tick) +: extrapolatePath(tail)
    case Node(floor, Some(GoDown), tick)::tail => Node(Math.min(floor - 1, MIN_FLOOR), callTick = tick) +: extrapolatePath(tail)
    case (node:Node)::tail => node +: extrapolatePath(tail)
  }


  def call(floor: Int, direction: Direction) = {
    status = status.copy(path = status.path :+ Node(floor, Some(direction)), shouldClose = false)
  }

  def go(floor: Int) = {
    status = status.copy(path = status.path :+ Node(floor), shouldClose = false)
  }

  def reset(cause: String) = {
    status = ElevatorStatus()
  }


}
