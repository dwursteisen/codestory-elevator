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

  sealed trait Operation {
    def isSameFloor(floor: Int):Boolean
  }

  case class Call(floor: Int, direction: Direction, callTick: Int = 0, go:Go = null) extends Operation {
    def toGo(floor: Int, goTick: Int = 0) = {
      Go(this, floor,goTick)
    }
    def isSameFloor(floor: Int): Boolean = (floor equals this.floor)
    def isSameOperation(operation:Call): Boolean = {
      isSameFloor(operation.floor) && (direction equals operation.direction) && (callTick equals operation.callTick)
    }
    def isSameOperation(operation:Go): Boolean = {
      false
    }
  }

  case class Go(call: Call, floor: Int, goTick: Int = 0) extends Operation {
    def isSameFloor(floor: Int): Boolean = (floor equals this.floor)
    def isSameOperation(operation:Call): Boolean = {
      false
    }
    def isSameOperation(operation:Go): Boolean = {
      isSameFloor(operation.floor) && call.isSameOperation(operation.call)
    }
  }

  case class ElevatorStatus(tick: Int = 0,
                    currentFloor: Int = 0,
                    currentDirection: Direction = GoUp,
                    currentStatus: Status = Closed,
                    path: Seq[Operation] = Seq(),
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
    val bestPath = bestPath(status)
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

//  def shortestPath(status: ElevatorStatus): Seq[Node] = status.path match {
//    case Nil => Seq()
//    case _ => {
//      val generatedPath = status.path.map(node => {
//          node +: shortestPath(status.copy(currentFloor = node.floor, path=status.path.diff(Seq(node))))
//      })
//      generatedPath.maxBy(p => scoreThisPath(status.currentFloor, p))
//    }
//  }

//  def pathToRoadMap(path: Seq[Node]): Seq[Int] = path match {
//    case Nil => Seq()
//    case head :: Nil => Seq(head.floor)
//    case head :: tail => head.floor +: pathToRoadMap(tail.filterNot(_.isSameFloor(head)))
//  }

  def pathToRoadMap(path: Seq[Operation]): Seq[Int] = path match {
    case Nil => Seq()
    case (head:Call) :: tail => head.floor +: pathToRoadMap(tail.filterNot(_.isSameFloor(head.floor)))
    case (head:Go) :: tail => head.floor +: pathToRoadMap(tail.filterNot(_.isSameFloor(head.floor)))
  }

  def scoreThisPath(currentFloor: Int, path: Seq[Node]): Double = path match {
    case Nil => 0.0
    case Node(floor, Some(GoUp), _) :: tail if floor == currentFloor => 5 + scoreThisPath(currentFloor+1, tail)
    case Node(floor, Some(GoDown), _) :: tail if floor == currentFloor => 5  + scoreThisPath(currentFloor-1, tail)
    case Node(floor, None, _) :: tail if floor == currentFloor => 10 + scoreThisPath(currentFloor, tail)
    case head :: tail if head.floor > currentFloor => -5 + scoreThisPath(currentFloor +1, path)
    case head :: tail if head.floor < currentFloor => -5 + scoreThisPath(currentFloor -1, path)
  }


  def bestPath(status:ElevatorStatus):Seq[Operation] = {
    val extrapolatedPath = extrapolatePath(status.path)

  }



  def extrapolatePath(path: Seq[Operation]): Seq[Operation] = path match {
    case Nil => Nil
    case (node:Go)::tail => node +:extrapolatePath(tail)
    case (node:Call)::tail if node.direction equals GoUp => {
      val go = node.toGo(Math.min(node.floor + 1, MAX_FLOOR))
      node.copy(go = go) +: go +: extrapolatePath(tail)
    }
    case (node:Call)::tail if node.direction equals GoDown => {
      val go = node.toGo(Math.max(node.floor - 1, MIN_FLOOR))
      node.copy(go = go) +: go +: extrapolatePath(tail)
    }
  }


  def tickIt(floor: Int, tick: Int, path:Seq[Operation], previousFloor:Int = -1): Seq[Operation] = path match {
    case Nil => Nil
    case(node:Go)::tail if node.isSameFloor(floor) && (floor == previousFloor) => node +: tickIt(floor, tick, tail, floor)
    case(node:Go)::tail if node.isSameFloor(floor) && (floor != previousFloor) => node +: tickIt(floor, tick + 2, tail, floor)
    case(node:Go)::tail if node.floor > floor => tickIt(floor + 1, tick + 1, path, floor)
    case(node:Go)::tail if node.floor < floor => tickIt(floor - 1, tick + 1, path, floor)
    case(node:Call)::tail if(node.floor > floor) => tickIt(floor + 1, tick+1, path, floor)
    case(node:Call)::tail if(node.floor < floor) => tickIt(floor - 1, tick+1, path, floor)
    case(node:Call)::tail if node.isSameFloor(floor) && (floor == previousFloor) => {
      node +: tickIt(floor, tick, replace(tail, node.go, node.go.copy(goTick=tick)), floor)
    }
    case(node:Call)::tail if node.isSameFloor(floor) && (floor != previousFloor) => {
      node +: tickIt(floor, tick+2, replace(tail, node.go, node.go.copy(goTick=tick+2)), floor)
    }
  }

  def scoreIt(floor:Int, tick: Int, path:Seq[Operation]):Double = path match {
    case Nil => 0
    case Call(callFloor, _, _, _) :: tail if (callFloor == floor) => scoreIt(floor, tick, tail)
    case Call(callFloor, _, _, _) :: tail if (floor > callFloor) => scoreIt(floor - 1, tick + 1, path)
    case Call(callFloor, _, _, _) :: tail if (floor < callFloor) => scoreIt(floor + 1, tick + 1, path)
    case Go(_, goFloor, _) :: tail if (floor > goFloor) => scoreIt(floor - 1, tick + 1, path)
    case Go(_, goFloor, _) :: tail if (floor < goFloor) => scoreIt(floor + 1, tick + 1, path)
    case Go(callOp, goFloor, goTick) :: tail if (floor == goFloor) => {

      val score = 20 - ((goTick-callOp.callTick) / 2) - (tick - goTick) + 2 + Math.abs(callOp.floor - goFloor)
      Math.min(Math.max(0, score), 20) + scoreIt(floor, tick, tail)
    }
  }

  def replace(path: Seq[Operation], to: Go, replaced: Go):Seq[Operation] = path match {
    case Nil => Nil
    case (op:Call)::tail => op +: replace(tail, to, replaced)
    case (op:Go) :: tail if op.isSameOperation(to) => replaced +: replace(tail, to, replaced)
    case (op:Go) :: tail if !op.isSameOperation(to) => op +: replace(tail, to, replaced)
  }

  def call(floor: Int, direction: Direction) = {
    status = status.copy(path = status.path :+ Call(floor, direction, status.tick), shouldClose = false)
  }

  def go(floor: Int) = {
    val callOp: Operation = status.path.find(op => op match {
      case (op: Call) if op.isSameFloor(status.currentFloor) && op.go == null => true
      case _ => false
    }).get

    val goOp: Go = callOp match {
      case (op:Call) => op.toGo(floor,status.tick)
    }

    status = status.copy(path = status.path :+ goOp, shouldClose = false)
  }

  def reset(cause: String) = {
    status = ElevatorStatus()
  }


}
