package services

import services.model._

/**
 * Created by david on 26/09/13.
 */
object Elevator {

  case class Node(floor: Int, direction: Option[Direction] = None) {
    def shouldFollowThisDirection(target: Node): Direction = {
      if (floor < target.floor) {
        GoUp
      } else if (floor > target.floor) {
        GoDown
      } else direction match {
        case Some(direction) => direction
        case None => GoUp // default
      }
    }

    def isSameFloor(other: Node): Boolean = other != null && isSameFloor(other.floor)
    def isSameFloor(floor: Int): Boolean = (floor equals this.floor)
  }

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
  var slowPath = Seq[Node]()
  var currentStatus: Status = Closed


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
        currentFloor = Math.min(currentFloor + 1, 6)
      }
      case Down :: tail  => {
        currentFloor = Math.max(currentFloor -1, 0)
      }
    }
    actions.headOption.getOrElse(Nothing)

  }

  def shortestPath(currentFloor: Int, currentDirection: Direction, roadmap: Seq[Node] = Seq()): Seq[Node] = roadmap match {
    case Nil => Seq()
    case _ => {
      val generatedPath = roadmap.map(node => {
          node +: shortestPath(node.floor, currentDirection, roadmap.diff(Seq(node)))
      })
      generatedPath.maxBy(p => scoreThisPath(currentFloor, p))
    }
  }

  def pathToRoadMap(path: Seq[Node]): Seq[Int] = path match {
    case Nil => Seq()
    case head :: Nil => Seq(head.floor)
    case head :: tail => head.floor +: pathToRoadMap(tail.filterNot(_.isSameFloor(head)))
  }

  def scoreThisPath(currentFloor: Int, path: Seq[Node]): Double = path match {
    case Nil => 0.0
    case Node(floor, Some(GoUp)) :: tail if floor == currentFloor => 10  + scoreThisPath(currentFloor+1, tail)
    case Node(floor, Some(GoDown)) :: tail if floor == currentFloor => 10  + scoreThisPath(currentFloor-1, tail)
    case Node(floor, None) :: tail if floor == currentFloor => 10  + scoreThisPath(currentFloor, tail)
    case head :: tail if head.floor > currentFloor => -5 + scoreThisPath(currentFloor +1, path)
    case head :: tail if head.floor < currentFloor => -5 + scoreThisPath(currentFloor -1, path)

  }


  def call(floor: Int, direction: Direction) = {
    slowPath = slowPath :+ Node(floor, Some(direction))
  }

  def go(floor: Int) = {
    slowPath = slowPath :+ Node(floor)
  }

  def reset(cause: String) = {
    currentFloor = 0
    currentDirection = GoUp
    slowPath = Seq[Node]()
    currentStatus = Closed
  }


}
