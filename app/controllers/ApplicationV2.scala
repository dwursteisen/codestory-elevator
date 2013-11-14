package controllers

import play.api.mvc._
import services.{StupidElevator, SimpleElevator, ShortestPathElevator}
import services.model._
import play.api.mvc.Action

object ApplicationV2 extends Controller {


  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }


  def call(atFloor: Int, to: String) = Action {
    val d = to match {
      case "UP" => GoUp
      case "DOWN" => GoDown
    }
    StupidElevator.call(atFloor, d)
    Ok
  }

  def go(floorToGo: Int) = Action {
    StupidElevator.go(floorToGo)
    Ok
  }

  def reset(cause: String, lowerFloor: Int, higherFloor: Int, cabinSize: Int) = Action {
    StupidElevator.reset(cause, lowerFloor,higherFloor, cabinSize)
    Ok
  }
  def userEntering() = Action {
    Ok
  }

  def userExiting() = Action {
    Ok
  }

  def nextCommand = Action {
    Ok(StupidElevator.nextCommand() match {
      case Open => "OPEN"
      case Close => "CLOSE"
      case Up => "UP"
      case Down => "DOWN"
      case _ => "NOTHING"
    })
  }

}