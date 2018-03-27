package controllers

import javax.inject.{Inject, _}

import io.lemonlabs.uri.dsl._
import models.{Challenge, SalesforceResponse}
import play.api.Configuration
import play.api.libs.json._
import play.api.libs.ws._
import play.api.mvc._
import play.api.Logger

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/** test
  * This controller creates an `Action` to handle HTTP requests to the
  * application's home page.
  */
@Singleton
class OAController @Inject()(cc: ControllerComponents, ws: WSClient, config: Configuration) extends AbstractController(cc) {

  implicit val salesforceResponseReads: Reads[SalesforceResponse] = Json.reads[SalesforceResponse]
  val TOKEN = "access_token"

  val URL = "instance_url"
  val ID = "id"
  val CHALLENGE = "challenge"

  val sfURL = config.getString("url").get
  val sfClientID = config.getString("client_id").get
  val sfSecret = config.getString("client_secret").get

  def redirect() = Action { implicit request: Request[AnyContent] =>
    val challenge = Challenge()
    Redirect(s"${sfURL}/services/oauth2/authorize" ?
      ("response_type" -> "code") &
      "client_id" -> sfClientID &
      "redirect_uri" -> routes.OAController.callback(None).absoluteURL() &
      "code_challenge" -> challenge.hash,
      302).withSession(CHALLENGE -> challenge.secret)
  }



  def callback(code: Option[String]) = Action.async { implicit request: Request[AnyContent] =>
    val uri = s"${sfURL}/services/oauth2/token"
    code.fold(Future {
      Unauthorized("Callback URL called without code.")
    }) { c =>
      ws.url(uri).post(
        Map(
          "grant_type" -> Seq("authorization_code"),
          "client_secret" -> Seq(sfSecret),
          "client_id" -> Seq(sfClientID),
          "redirect_uri" -> Seq(routes.OAController.callback(None).absoluteURL().toString), //Bug in docs where redirect_uri is called redirect_url
          "code" -> Seq(c),
          "code_verifier" -> Seq(request.session.get(CHALLENGE).getOrElse(""))
        ))
        .map {_.json.validate[SalesforceResponse]}
        .map {
          case r: JsSuccess[SalesforceResponse] => {
            Redirect(routes.OAController.doSomething(), FOUND).withSession(
              request.session +
                (TOKEN -> r.value.access_token) +
                (URL -> r.value.instance_url) +
                (ID -> r.value.id)
            )
          }
          case e: JsError => {
            Logger.error(s"Could not parse json error: ${e.errors.mkString}")
            Unauthorized("Could not parse response")
          }
        }
    }
  }
  def doSomething() = Action.async { implicit request: Request[AnyContent] =>
    val a = for {
      id <- request.session.get(ID)
      token <- request.session.get(TOKEN)
      url <- request.session.get(URL)
    } yield {
      (id, token, url)
    }
    a.fold(Future {
      Redirect(routes.HomeController.index().absoluteURL(), FORBIDDEN)
    }) { case (id: String, token: String, url: String) =>
      ws.url(id).addHttpHeaders(("Authorization", s"Bearer ${token}")).get().map { r =>
        Ok(r.body)
      }
    }

  }
}
