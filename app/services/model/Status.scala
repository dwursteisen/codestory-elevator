package services.model

/**
 * Created by david on 27/09/13.
 */
sealed trait Status
object Opened extends Status {
  override def toString: String = "Doors Opened"
}
object Closed extends Status {
  override def toString: String = "Doors Closed"
}
