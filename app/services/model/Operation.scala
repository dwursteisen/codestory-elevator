package services.model

/**
 * Created by david on 01/10/13.
 */
trait Operation {
  def floor: Int
  def cabin: Int

  def isSameFloor(floor: Int): Boolean = (floor equals this.floor)
  def isSameFloor(op: Operation): Boolean = isSameFloor(op.floor)
  def isSameCabin(cabin: Int): Boolean = (cabin equals this.cabin)
}

case class Call(floor: Int, cabin: Int, direction: Direction) extends Operation

case class Go(floor: Int, cabin: Int) extends Operation
