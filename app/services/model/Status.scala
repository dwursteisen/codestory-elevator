package services.model

/**
 * Created by david on 27/09/13.
 */
sealed trait Status
object Opened extends Status
object Closed extends Status
