package com.liftcode.unconference.snippet

import com.liftcode.unconference._
import model._

import net.liftweb._
import http._
import S._
import SHtml._
import util._
import Helpers._
import mapper._

import scala.xml.{NodeSeq, Text, MetaData, Null}

class Register extends StatefulSnippet {
  var dispatch: DispatchIt = {
    case "form" => html => firstForm(chooseTemplate("register", "first", html))
  }
  
  var firstName = ""
  var lastName = ""
  var email1 = ""
  var email2 = ""
  var pwd1 = ""
  var pwd2 = ""
  var edu = false
  
  var firstName_err = false
  var lastName_err = false
  var email1_err = false
  var email2_err = false
  var pwd1_err = false
  var pwd2_err = false
  var user: User = _
  
  private def validate() {
    if (firstName.length < 2) {firstName_err = true; error("First Name too short")}
    else firstName_err = false
    
    if (lastName.length < 3) {lastName_err = true; error("Last Name too short")}
    else lastName_err = false
    
    if (!MappedEmail.validEmailAddr_?(email1)) {email1_err = true; error("Invalid email address")}
    else email1_err = false
    
    email2_err = false
    
    if (!email1_err && !email2_err && email1 != email2) {
      email1_err = true
      email2_err = true
      error("Email addresses do not match")
    }

    if (!email1_err && !email2_err && User.find(By(User.email, email1)).isDefined) {
      email1_err = true
      email2_err = true
      error("This email address has already been registered")
    }
    
    
    if (pwd1.length < 6) {pwd1_err = true; error("Password too short")}
    else pwd1_err = false
    
    pwd2_err = false
    
    if (!pwd1_err && !pwd2_err && pwd1 != pwd2) {
      pwd1_err = true
      pwd2_err = true
      error("Passwords do not match")
    }
    
    if (!firstName_err && !lastName_err && !email1_err && !email2_err &&
    !pwd1_err && !pwd2_err) {
      user = User.create.firstName(firstName).lastName(lastName).email(email1).password(pwd1).makeWikiName().saveMe
      if (edu) dispatch = {case "form" => html => confirmEdu(chooseTemplate("register", "edu", html))}
      else dispatch = {case "form" => html => payPal(chooseTemplate("register", "paypal", html))}
    }
  }
  
  def ie(in: Boolean): MetaData = if (in) ("class" -> "input_error")
  else Null
  
  def firstForm(body: NodeSeq): NodeSeq = 
  <form method="POST" action={S.uri}>
  {
    SHtml.hidden(() => this.registerThisSnippet)
  }
  {
  bind("register", body,
  "firstName" --> text(firstName, s => firstName = s.trim) % ie(firstName_err),
  "lastName" --> text(lastName, s => lastName = s.trim) % ie(lastName_err),
  "email1" --> text(email1, s => email1 = s.toLowerCase.trim) % ie(email1_err),
  "email2" --> text(email2, s => email2 = s.toLowerCase.trim) % ie(email2_err),
  "pwd1" --> password(pwd1, s => pwd1 = s.trim) % ie(pwd1_err),
  "pwd2" --> password(pwd2, s => pwd2 = s.trim) % ie(pwd2_err),
  "edu" --> checkbox(edu, edu = _),
  "submit" --> submit("Register", validate _)
  )
  }</form>
  
  def confirmEdu(body: NodeSeq): NodeSeq = body
  
  def payPal(body: NodeSeq): NodeSeq = bind("register", body,
  "item_number" --> <input type="hidden" name="item_number" value={user.paypalId.is}/>)
}


