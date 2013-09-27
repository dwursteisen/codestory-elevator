package services

import services.model._

/**
 * Created by david on 26/09/13.
 */
object Elevator {

  case class Node(floor: Int, direction: Option[Direction] = None)

  def toActions(currentFloor: Int, currentStatus: Status, roadmap: Seq[Int]) : Seq[Action] = {
    roadmap match {
      case Nil => Nil
      case head::tail if head equals currentFloor => currentStatus match {
        case Closed => Open +: toActions(currentFloor, Opened, tail)
        case Opened => Nothing +: toActions(currentFloor, Opened, tail)
      }
      case head::tail if currentFloor < head => currentStatus match {
        case Closed => Up +: toActions(currentFloor + 1, Closed, roadmap)
        case Opened => Close +: toActions(currentFloor, Closed, roadmap)
      }
      case head::tail if currentFloor > head => currentStatus match {
        case Closed => Down +: toActions(currentFloor - 1, Closed, roadmap)
        case Opened => Close +: toActions(currentFloor, Closed, roadmap)
      }
    }
  }


}
