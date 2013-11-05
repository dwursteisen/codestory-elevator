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

  def go(floorToGo: Int) = Action {
    SimpleElevator.go(floorToGo)
    Ok
  }

  def reset(cause: String) = Action {
    SimpleElevator.reset(cause)
    Ok
  }
  def userEntering() = Action {
    Ok
  }

  def userExiting() = Action {
    Ok
  }

  def nextCommand = Action {
    Ok(SimpleElevator.nextCommand().toString)
  }

}