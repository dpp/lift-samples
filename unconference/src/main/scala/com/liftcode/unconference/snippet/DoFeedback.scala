package com.liftcode.unconference.snippet

import net.liftweb._
import http._
import util._
import Helpers._

import com.liftcode.unconference._
import model._

import scala.xml._

class DoFeedback extends StatefulSnippet {
  def dispatch = {
    case _ => doIt
  }
  
  val fb = Feedback.create.author(User.currentUser).
  email(User.currentUser.map(_.email.is).openOr(""))
  
  val referer: Can[String] = S.referer
  
  private def submitIt(ignore: String) {
    fb.validate match {
      case Nil => S.notice("Thanks for your feedback!")
      fb.save
      referer.foreach(S.redirectTo(_))
      S.redirectTo("/")
      
      case issues => S.error(issues)
      
    }
  }
  
  def doIt(in: NodeSeq): NodeSeq = 
  bind("feedback", in,
  "email" --> User.currentUser.map(u => <div>{u.email}</div>).
  openOr(SHtml.text(fb.email, s => fb.email(s))),
  "feedback" --> SHtml.textarea(fb.text, s => fb.text(s)),
  "submit" --> SHtml.submit("Please tell us", submitIt)
  )
}

