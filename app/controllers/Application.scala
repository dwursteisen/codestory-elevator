package controllers

import play.api.mvc._
import services.ShortestPathElevator
import services.model._
import play.api.mvc.Action

object Application extends Controller {


  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }


  def call(atFloor: Int, to: String) = Action {
    val d = to match {
      case "UP" => GoUp
      case "DOWN" => GoDown
    }
    ShortestPathElevator.call(atFloor, d)
    Ok
  }

  def go(floorToGo: Int) = Action {
    ShortestPathElevator.go(floorToGo)
    Ok
  }

  def reset(cause: String) = Action {
    ShortestPathElevator.reset(cause)
    Ok
  }
  def userEntering() = Action {
    Ok
  }

  def userExiting() = Action {
    Ok
  }

  def nextCommand = Action {
    Ok(ShortestPathElevator.nextCommand() match {
      case Open => "OPEN"
      case Close => "CLOSE"
      case Up => "UP"
      case Down => "DOWN"
      case _ => "NOTHING"
    })
  }

}