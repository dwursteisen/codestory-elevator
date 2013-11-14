package services

import services.model._

/**
 * Created by david on 01/10/13.
 */
object StupidElevator extends Elevator {

  val actions = Seq(
    Open, Close, Up,
    Open, Close, Up,
    Open, Close, Up,
    Open, Close, Up,
    Open, Close, Up,
    Open, Close, Up,
    Open, Close, Down,
    Open, Close, Down,
    Open, Close, Down,
    Open, Close, Down,
    Open, Close, Down,
    Open, Close, Down
  )

  var index:Int = 0

  def nextCommand(): Action = {
    val action = actions(index)
    index = (index + 1) % actions.size
    action
  }

  def call(floor: Int, direction: Direction) {}

  def go(floor: Int) {}

  def reset(cause: String, lowerFloor: Int, higherFloor: Int, cabinSize: Int) {
    // I don't give a fuck
  }
}
