package services.model

/**
 * Created by david on 01/10/13.
 */
trait Operation {
  def floor: Int

  def isSameFloor(floor: Int): Boolean = (floor equals this.floor)
  def isSameFloor(op: Operation): Boolean = isSameFloor(op.floor)
}

case class Call(floor: Int, direction: Direction) extends Operation

case class Go(floor: Int) extends Operation
