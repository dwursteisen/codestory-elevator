package services.model

/**
 * Created by david on 27/09/13.
 */
sealed trait Action

case object Nothing extends Action {
  override def toString: String = "NOTHING"
}

case object Up extends Action {
  override def toString: String = "UP"
}

case object Down extends Action {
  override def toString: String = "DOWN"
}

case object Open extends Action {
  override def toString: String = "OPEN"
}

case object OpenUp extends Action {
  override def toString: String = "OPEN_UP"

}

case object OpenDown extends Action {
  override def toString: String = "OPEN_DOWN"

}
case object Close extends Action {
  override def toString: String = "CLOSE"
}

