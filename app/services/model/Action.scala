package services.model

/**
 * Created by david on 27/09/13.
 */
sealed trait Action

case object Nothing extends Action

case object Up extends Action

case object Down extends Action

case object Open extends Action

case object Close extends Action
