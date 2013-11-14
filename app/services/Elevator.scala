package services

import services.model._

/**
 * Created by david on 01/10/13.
 */
trait Elevator {

  val MAX_FLOOR = 19
  val MIN_FLOOR = 0

  def nextCommand(): Action

  def call(floor: Int, direction: Direction)

  def go(floor: Int)

  def reset(cause: String, lowerFloor: Int, higherFloor: Int, cabinSize: Int)
}
