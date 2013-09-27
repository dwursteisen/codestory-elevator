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

    def isSameFloor(other: Node): Boolean = other.floor equals floor
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


  def shortestPath(currentFloor: Int, currentDirection: Direction, roadmap: Seq[Node] = Seq()): Seq[Node] = {
    val start: Node = Node(currentFloor, Some(currentDirection))

    val generatedPath = roadmap.map(node => {
      shortestPath(node.floor, start.shouldFollowThisDirection(node), roadmap.diff(Seq(node)))
    })

    generatedPath.maxBy(scoreThisPath(_))
  }

  def pathToRoadMap(path: Seq[Node]): Seq[Int] = path match {
    case head::Nil => Seq(head.floor)
    case head::tail => head.floor +: pathToRoadMap(tail.dropWhile(_.isSameFloor(head)))
  }

  def scoreThisPath(path: Seq[Node]): Double = {
    def _scoreThisPath(path: Seq[Node], malus: Double): Double = path match {
      case Nil => 0
      case head :: tail => {
        val nodeScore: Double = 100 * malus * path.count(_.isSameFloor(head))
        nodeScore + _scoreThisPath(tail.dropWhile(_.isSameFloor(head)), malus / path.size)
      }
    }
    _scoreThisPath(path, 1)
  }


}
