package services

import services.model._

/**
 * Created by david on 01/10/13.
 */
trait Elevator {

  def nextCommand(): Action

  def call(floor: Int, direction: Direction)

  def go(floor: Int)

  def reset(cause: String)
}
