package com.liftcode.unconference.snippet

import com.liftcode.unconference._
import model._

import net.liftweb._
import http._
import S._
import util._
import Helpers._
import mapper._
import Mailer._

import scala.xml.{NodeSeq, Text, MetaData, Null}

class SuperPower extends StatefulSnippet {
  var dispatch: DispatchIt = {
    case _ => beSuper
  }
  
  private var msg = ""
  private var subject = ""

  private def validatePaypal(id: String) {
    User.find(By(User.paypalId, id.trim)) match {
      case Full(user) => user.validated(true).save
      S.notice(user.niceName+" has been validated")
      
      case _ => S.error("PayPal ID "+id+" not found")
    }
  }
  
  private def sendMsg(to: String) {
    val users = User.findAll.filter{
      case u if to == "All" => true
      case u if to == "Validated" && u.validated.is == true => true
      case u if to == "Unvalidated" && u.validated.is != true => true
      case _ => false
    }
    
    for (u <- users) 
     Mailer.sendMail(From("feedback@scalaliftoff.com"),Subject(subject), 
     To(u.email), msg)
    
    S.notice("Sent msg to "+users.length)
  }
  
  def beSuper(in: NodeSeq): NodeSeq = 
  if (User.superUser_?)
  <div>
  <p>Super Powers... use them <b>carefully</b></p>
    <br/>
  <p>
  Validate a PayPal purchase
  <form action={S.uri} method="POST">
  PayPal magic number: {S.text("", validatePaypal _)} 
  <input type="submit" value="Validate" />
  </form>
  </p>
  <br/>
  <p>
  Send email: <br />
  <form action={S.uri} method="POST">
  Subject: {S.text(subject, subject = _)}<br/>
  {S.textarea(msg, msg = _)}<br/>
  To: {S.select(List("All", "Validated", "Unvalidated").map(s => (s,s)), Full("All"), sendMsg)}<br/>
  <input type="submit" value="Send"/>
  </form>
  </p>
  </div>
  else
  S.redirectTo("/")
}


