package services.model

/**
 * Created by david on 27/09/13.
 */
sealed trait Direction {
  def flip():Direction
}

case object GoUp extends Direction{
  def flip(): Direction = GoDown
}

case object GoDown extends Direction{
  def flip(): Direction = GoUp
}
