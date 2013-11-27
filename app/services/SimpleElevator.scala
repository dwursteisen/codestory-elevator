package services

import services.model._
import play.api.Logger
import org.joda.time.DateTime

/**
 * Created by david on 01/10/13.
 */
object SimpleElevator extends Elevator {

  var path: Seq[Operation] = Seq()

  case class CurrentStatus(cabin: Int, floor: Int = 0, passenger: Int = 0, status: Status = Closed, direction: Option[Direction] = None) {
    def up(): CurrentStatus = {
      this.copy(floor = floor + 1, direction = Some(GoUp))
    }

    def down(): CurrentStatus = {
      this.copy(floor = floor - 1, direction = Some(GoDown))
    }

    def open(): CurrentStatus = {
      this.copy(status = Opened)
    }

    def close(): CurrentStatus = {
      this.copy(status = Closed)
    }
  }


  case class CurrentConfig(maxFloor: Int = 19, minFloor: Int = 0, maxPassenger: Int = 40, cabinCount: Int = 2)


  var config: CurrentConfig = CurrentConfig()
  var currents: Seq[CurrentStatus] = allCabins.map(c => CurrentStatus(c))

  def nextCommands(): Seq[Action] = {
    for {
      cabin <- currents
    } yield nextCommand(cabin)
  }

  def allCabins = (0.to(config.cabinCount - 1))

  def nextCommand(current: CurrentStatus): Action = {
    val cabinPath = operationsForThisCabin(this.path, current)
    val closestOperation = findNextOperation(cabinPath, current,
      nextDirection(cabinPath, current, current.direction))
    val result = closestOperation match {
      case None => {
        Nothing

      }
      case Some(operation) => {
        val action = toAction(current, operation)
        val nextCurrent = applyOperation(current, operation)
        // replace the old with the new
        currents = replace(current).withThis(nextCurrent).into(currents)
        action
      }

    }

    Logger.info({
      "%s /nextCommand %s".format(DateTime.now(), result)
    })
    result
  }

  def replace(old: CurrentStatus) = {
    class With {
      def withThis(next: CurrentStatus) = {

        class Into {
          def into(currents: Seq[CurrentStatus]): Seq[CurrentStatus] = {
            (currents.filterNot(old eq _) :+ next).sortBy(_.cabin)
          }
        }
        new Into()
      }


    }
    new With()
  }

  def findNextOperation(path: Seq[Operation], current: CurrentStatus, direction: Option[Direction]): Option[Operation] = direction match {
    case Some(GoUp) => operationsInThisDirection(path, current, GoUp).filter(_.floor >= current.floor).sortBy(_.floor).headOption match {
      case None => findNextOperation(path, current, None)
      case option => option
    }
    case Some(GoDown) => operationsInThisDirection(path, current, GoDown).filter(_.floor <= current.floor).sortBy(-_.floor).headOption match {
      case None => findNextOperation(path, current, None)
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

  def operationsForThisCabin(path: Seq[Operation], current: CurrentStatus) = path.filter(_.isSameCabin(current.cabin))

  def operationsThatCanFillThisCabin(path: Seq[Operation], current: CurrentStatus) = {

  }

  def operationsInThisDirection(path: Seq[Operation], current: CurrentStatus, direction: Direction) = direction match {
    case GoUp => path.filter(op => op match {
      case Call(_, cabin, GoDown) => false
      case _ => true
    })
    case GoDown => path.filter(op => op match {
      case Call(_, cabin, GoUp) => false
      case _ => true
    })
  }

  def nextDirection(path: Seq[Operation], current: CurrentStatus, direction: Option[Direction]): Option[Direction] = direction match {
    case Some(GoUp) => if (operationsInThisDirection(path, current, GoUp).filter(_.floor >= current.floor).size > 0) {
      Some(GoUp)
    } else {
      nextDirection(path, current, Some(GoDown))
    }
    case Some(GoDown) => if (operationsInThisDirection(path, current, GoDown).filter(_.floor <= current.floor).size > 0) {
      Some(GoDown)
    } else {
      nextDirection(path, current, None)
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

  def toAction(current: CurrentStatus, operation: Operation): Action = current.status match {
    case Closed if operation.isSameFloor(current.floor) => current.direction.map(_ match {
      case GoUp => OpenUp
      case GoDown => OpenDown
    }).getOrElse(Open)
    case Closed if operation.floor > current.floor => Up
    case Closed if operation.floor < current.floor => Down
    case Opened if operation.isSameFloor(current.floor) => Close
    case Opened if !operation.isSameFloor(current.floor) => Close

  }

  def applyOperation(current: CurrentStatus, operation: Operation): CurrentStatus = current.status match {
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
    val ops: Seq[Operation] = allCabins.map(c => Call(floor, c, direction))
    path = path ++ ops
  }

  def go(floor: Int, cabin: Int) {
    path = path :+ Go(floor, cabin)
  }

  def reset(cause: String, lowerFloor: Int, higherFloor: Int, cabinSize: Int, cabinCount: Int) {
    path = Seq()
    config = new CurrentConfig(maxFloor = higherFloor, minFloor = lowerFloor, maxPassenger = cabinSize, cabinCount = cabinCount)
    currents = allCabins.map(c => CurrentStatus(c))
    Logger.error({
      "%s /reset %s".format(DateTime.now(), cause)
    })
    Logger.info({
      "%s /status %s ".format(DateTime.now(), currents)
    })
  }

  def userHasEntered(cabin: Int): Unit = {
    val cabin: Option[CurrentStatus] = currents.find(_.cabin == cabin)
    cabin match {
      case None => ()
      case Some(current) => {
          currents = replace(current).withThis(current.copy(passenger = Math.min(config.maxPassenger, current.passenger + 1))).into(currents)
      }
    }
  }

  def userHasExited(cabin: Int): Unit ={
    val cabin: Option[CurrentStatus] = currents.find(_.cabin == cabin)
    cabin match {
      case None => ()
      case Some(current) => {
          currents = replace(current).withThis(current.copy(passenger = Math.max(0, current.passenger - 1))).into(currents)
      }
    }
  }
}
