package services

import services.model._

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
    closestOperation match {
      case None => {
        Nothing
      }
      case Some(operation) => {
        val action = toAction(operation)
        currentDirection = nextDirection(currentDirection)
        action
      }
    }

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
    case Closed if operation.isSameFloor(currentFloor) => {
      path = path.filterNot(_.isSameFloor(currentFloor))
      currentStatus = Opened
      shouldClose = false
      Open
    }
    case Closed if operation.floor > currentFloor => {
      currentFloor = currentFloor + 1
      Up
    }
    case Closed if operation.floor < currentFloor => {
      currentFloor = currentFloor - 1
      Down
    }
    case Opened if operation.isSameFloor(currentFloor) && shouldClose == false => {
      path = path.filterNot(_.isSameFloor(currentFloor))
      shouldClose = true
      Nothing
    }
    case Opened if operation.isSameFloor(currentFloor) && shouldClose == true => {
      path = path.filterNot(_.isSameFloor(currentFloor))
      currentStatus = Closed
      shouldClose = false
      Close
    }
    case Opened if !operation.isSameFloor(currentFloor) => {
      currentStatus = Closed
      shouldClose = false
      Close
    }

  }

  def call(floor: Int, direction: Direction) {
    path = path :+ Call(floor, direction)
    shouldClose = false
  }

  def go(floor: Int) {
    path = path :+ Go(floor)
    shouldClose = false
  }

  def reset(cause: String) {
    currentDirection = None
    currentFloor = 0
    currentStatus = Closed
    path = Seq()
    shouldClose = false
  }
}
