package controllers

import play.api.mvc._
import services.SimpleElevator
import services.model._
import play.api.mvc.Action

object ApplicationV3 extends Controller {


  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }


  def call(atFloor: Int, to: String) = Action {
    val d = to match {
      case "UP" => GoUp
      case "DOWN" => GoDown
    }
    SimpleElevator.call(atFloor, d)
    Ok
  }

  def go(floorToGo: Int, cabin: Int) = Action {
    SimpleElevator.go(floorToGo, cabin)
    Ok
  }

  def reset(cause: String, lowerFloor: Int, higherFloor: Int, cabinSize: Int, cabinCount: Int) = Action {
    SimpleElevator.reset(cause, lowerFloor,higherFloor, cabinSize, cabinCount)
    Ok
  }
  def userEntering() = Action {
    Ok
  }

  def userExiting() = Action {
    Ok
  }

  def nextCommands = Action {
    Ok(SimpleElevator.nextCommands().foldLeft("")((e1, e2) => e1 + e2.toString + "\n"))
  }

}