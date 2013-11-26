package controllers

import play.api.mvc._
import services.model._
import play.api.mvc.Action

object Application extends Controller {


  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }


}