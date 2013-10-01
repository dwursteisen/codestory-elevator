package services

import services.model.{Action, Direction}

/**
 * Created by david on 01/10/13.
 */
object SimpleElevator extends Elevator{
  def nextCommand(): Action = ???

  def call(floor: Int, direction: Direction) {}

  def go(floor: Int) {}

  def reset(cause: String) {}
}
