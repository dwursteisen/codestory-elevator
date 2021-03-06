package services

import services.model._
import services.SimpleElevator.CurrentStatus

/**
 * Created by david on 01/10/13.
 */
trait Elevator {

  val MAX_FLOOR = 19
  val MIN_FLOOR = 0

  def nextCommand(current: CurrentStatus): Action
  def nextCommands(): Seq[Action]

  def call(floor: Int, direction: Direction)

  def go(floor: Int, cabin: Int)

  def reset(cause: String, lowerFloor: Int, higherFloor: Int, cabinSize: Int, cabinCount: Int)

  def userHasEntered(cabin: Int)
  def userHasExited(cabin: Int)
}
