package services.model

/**
 * Created by david on 27/09/13.
 */
sealed trait Direction

case object GoUp extends Direction

case object GoDown extends Direction
