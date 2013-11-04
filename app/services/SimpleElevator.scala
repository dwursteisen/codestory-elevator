package services

import services.model._
import play.api.Logger

/**
 * Created by david on 01/10/13.
 */
object SimpleElevator extends Elevator {

  var currentDirection: Option[Direction] = None
  var currentFloor: Int = 0
  var currentStatus: Status = Closed
  var path: Seq[Operation] = Seq()
  var shouldClose = false

  def nextCommand(): Action = {
    val closestOperation = findNextOperation(currentDirection)
    val result = closestOperation match {
      case None => {
        Nothing
      }
      case Some(operation) => {
        val action = toAction(operation)
        applyOperation(operation)
        currentDirection = nextDirection(currentDirection)
        action
      }

    }

    Logger.info({"NEXT COMMAND %s".format(result)})
    result
  }


  def findNextOperation(direction: Option[Direction]): Option[Operation] = direction match {
    case Some(GoUp) => operationsInThisDirection(GoUp).filter(_.floor >= currentFloor).sortBy(_.floor).headOption match {
      case None => findNextOperation(None)
      case option => option
    }
    case Some(GoDown) => operationsInThisDirection(GoDown).filter(_.floor <= currentFloor).sortBy(-_.floor).headOption match {
      case None => findNextOperation(None)
      case option => option
    }
    case None => {
      val below = path.filter(_.floor <= currentFloor)
      val above = path.filter(_.floor >= currentFloor)
      if (below.size > above.size) {
        below.sortBy(_.floor).headOption
      } else {
        above.sortBy(-_.floor).headOption
      }
    }
  }

  def operationsInThisDirection(direction: Direction) = direction match {
    case GoUp => path.filter(op => op match {
      case Call(_, GoDown) => false
      case _ => true
    })
    case GoDown => path.filter(op => op match {
      case Call(_, GoUp) => false
      case _ => true
    })
  }

  def nextDirection(direction: Option[Direction]): Option[Direction] = direction match {
    case Some(GoUp) => if (operationsInThisDirection(GoUp).filter(_.floor >= currentFloor).size > 0) {
      Some(GoUp)
    } else {
      nextDirection(Some(GoDown))
    }
    case Some(GoDown) => if (operationsInThisDirection(GoDown).filter(_.floor <= currentFloor).size > 0) {
      Some(GoDown)
    } else {
      nextDirection(None)
    }
    case None => {
      val below = path.filter(_.floor <= currentFloor)
      val above = path.filter(_.floor >= currentFloor)
      if (below.size > above.size) {
        Some(GoDown)
      } else {
        Some(GoUp)
      }
    }


  }

  def toAction(operation: Operation): Action = currentStatus match {
    case Closed if operation.isSameFloor(currentFloor) => Open
    case Closed if operation.floor > currentFloor => Up
    case Closed if operation.floor < currentFloor => Down
    case Opened if operation.isSameFloor(currentFloor) && shouldClose == false => Nothing
    case Opened if operation.isSameFloor(currentFloor) && shouldClose == true => Close
    case Opened if !operation.isSameFloor(currentFloor) => Close

  }

  def applyOperation(operation: Operation) = currentStatus match {
    case Closed if operation.isSameFloor(currentFloor) => {
      path = path.filterNot(_.isSameFloor(currentFloor))
      currentStatus = Opened
      shouldClose = false
    }
    case Closed if operation.floor > currentFloor => {
      currentFloor = currentFloor + 1
    }
    case Closed if operation.floor < currentFloor => {
      currentFloor = currentFloor - 1
    }
    case Opened if operation.isSameFloor(currentFloor) && shouldClose == false => {
      path = path.filterNot(_.isSameFloor(currentFloor))
      shouldClose = true
    }
    case Opened if operation.isSameFloor(currentFloor) && shouldClose == true => {
      path = path.filterNot(_.isSameFloor(currentFloor))
      currentStatus = Closed
      shouldClose = false
    }
    case Opened if !operation.isSameFloor(currentFloor) => {
      currentStatus = Closed
      shouldClose = false
    }

  }

  def call(floor: Int, direction: Direction) {
    path = path :+ Call(floor, direction)
    shouldClose = false
    Logger.info({"CALL %d %s".format(floor, direction)})
  }

  def go(floor: Int) {
    path = path :+ Go(floor)
    shouldClose = false
    Logger.info({"GO %d".format(floor)})
  }

  def reset(cause: String) {
    currentDirection = None
    currentFloor = 0
    currentStatus = Closed
    path = Seq()
    shouldClose = false

    Logger.error({"RESET %s".format(cause)})
  }
}
