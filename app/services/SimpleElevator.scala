package services

import services.model._
import play.api.Logger
import org.joda.time.DateTime

/**
 * Created by david on 01/10/13.
 */
object SimpleElevator extends Elevator {

  var path: Seq[Operation] = Seq()
  
  case class CurrentStatus(floor: Int = 0, status: Status = Closed, direction: Option[Direction] = None) {
      def up(): CurrentStatus = {
          this.copy(floor = floor + 1, direction = Some(GoUp))
      }

      def down(): CurrentStatus = {
          this.copy(floor = floor - 1, direction = Some(GoDown))
      }

      def open(): CurrentStatus = {
        this.copy(status = Opened)
      }

      def close():CurrentStatus = {
        this.copy(status = Closed)
      }
  }


  case class CurrentConfig(maxFloor: Int = 19, minFloor: Int = 0, maxPassenger: Int = 40, cabinCount: Int = 2)

  var current: CurrentStatus = CurrentStatus()
  var config: CurrentConfig = CurrentConfig()

  def nextCommands(): Seq[Action] = {
    for {
      cabin <- (0.to (config.cabinCount - 1))
    } yield nextCommand(cabin)
  }

  def nextCommand(cabin: Int): Action = {
    val closestOperation = findNextOperation(nextDirection(current.direction))
    val result = closestOperation match {
      case None => {
        Nothing
        
      }
      case Some(operation) => {
        val action = toAction(operation)
        current = applyOperation(operation)
        action
      }

    }

    Logger.info({"%s /nextCommand %s".format(DateTime.now(), result)})
    // Logger.info({"%s /status %s ".format(DateTime.now(), current)})
    result
  }


  def findNextOperation(direction: Option[Direction]): Option[Operation] = direction match {
    case Some(GoUp) => operationsInThisDirection(GoUp).filter(_.floor >= current.floor).sortBy(_.floor).headOption match {
      case None => findNextOperation(None)
      case option => option
    }
    case Some(GoDown) => operationsInThisDirection(GoDown).filter(_.floor <= current.floor).sortBy(-_.floor).headOption match {
      case None => findNextOperation(None)
      case option => option
    }
    case None => {
      val below = path.filter(_.floor <= current.floor)
      val above = path.filter(_.floor >= current.floor)
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
    case Some(GoUp) => if (operationsInThisDirection(GoUp).filter(_.floor >= current.floor).size > 0) {
      Some(GoUp)
    } else {
      nextDirection(Some(GoDown))
    }
    case Some(GoDown) => if (operationsInThisDirection(GoDown).filter(_.floor <= current.floor).size > 0) {
      Some(GoDown)
    } else {
      nextDirection(None)
    }
    case None => {
      val below = path.filter(_.floor <= current.floor)
      val above = path.filter(_.floor >= current.floor)
      if (below.size > above.size) {
        Some(GoDown)
      } else {
        Some(GoUp)
      }
    }


  }

  def toAction(operation: Operation): Action = current.status match {
    case Closed if operation.isSameFloor(current.floor) => Open
    case Closed if operation.floor > current.floor => Up
    case Closed if operation.floor < current.floor => Down
    case Opened if operation.isSameFloor(current.floor) => Close
    case Opened if !operation.isSameFloor(current.floor) => Close

  }

  def applyOperation(operation: Operation):CurrentStatus = current.status match {
    case Closed if operation.isSameFloor(current.floor) => {
      path = path.filterNot(_.isSameFloor(current.floor))
      current.open()
    }
    case Closed if operation.floor > current.floor => {
      current.up()
    }
    case Closed if operation.floor < current.floor => {
      current.down()
    }
    case Opened if operation.isSameFloor(current.floor) => {
      path = path.filterNot(_.isSameFloor(current.floor))
      current.close()
    }
    case Opened if !operation.isSameFloor(current.floor) => {
      current.close()
    }

  }

  def call(floor: Int, direction: Direction) {
    path = path :+ Call(floor, direction)
    // Logger.info({"%s /call?atFloor=%d&to=%s".format(DateTime.now(), floor, direction)})
    // Logger.info({"%s /status %s ".format(DateTime.now(), current)})

  }

  def go(floor: Int, cabin: Int) {
    path = path :+ Go(floor)
    // Logger.info({"%s /go?floorToGo=%d".format(DateTime.now(), floor)})
    // Logger.info({"%s /status %s ".format(DateTime.now(), current)})
  }

  def reset(cause: String, lowerFloor: Int, higherFloor: Int, cabinSize: Int, cabinCount: Int) {
    path = Seq()
    current = new CurrentStatus()
    config = new CurrentConfig(maxFloor = higherFloor, minFloor = lowerFloor, maxPassenger = cabinSize, cabinCount = cabinCount)
    Logger.error({"%s /reset %s".format(DateTime.now(), cause)})
    Logger.info({"%s /status %s ".format(DateTime.now(), current)})
  }
}
