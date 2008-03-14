package com.liftcode.unconference.snippet

import com.liftcode._
import model._

import net.liftweb._
import http._
import S._
import util._
import Helpers._
import mapper._

import scala.xml.{NodeSeq, Text}

class SignIn extends StatefulSnippet {
  def dispatch = {
    case "display_login" => displayLogin
  }
  
  def displayLogin(in: NodeSeq) = User.currentUser match {
    case Full(user) =>  
    <div class="column">
    <h3 class="red">Welcome {user.firstName}</h3>
    {
      if (!user.validated) <p class="block">Your account has not been validated yet</p>
      else if (user.superUser) <p class="block"><a href="/super">Super Powers</a></p>
      else <p class="block"><a href={"/community/edit/"+urlEncode(user.wikiName)}>Tell us about you</a></p>
    }
    <p class="block"><a href="/logout">Logout</a></p>
    </div>
    case _ => in
  }
}

object Login {
  def login(r: RequestState) = {
    val from = S.referer.openOr("/")
    
    User.currentUser match {
      case Full(_) => // do nothing
      case _ => 
      def testPwd(user: User, pwd: String): Can[Boolean] = 
      if (user.password.match_?(pwd)) {
        if (user.invalid_?) Failure(user.invalidReason, Empty, Nil)
        else {User.logUserIn(user); Full(true)}
        
        //Full(true)
      } else Failure("Password mis-match", Empty, Nil)
      
      (for (email <- S.param("username") ?~ "No Username";
      pwd <- S.param("password") ?~ "No Password";
      user <- User.find(By(User.email, email)) ?~ "User Not Found";
      success <- testPwd(user, pwd)) yield {
        user
      }) match {
        case Full(user) => notice("Welcome: "+user.niceName)
        if (!user.validated) {
          notice("Until your registration is confirmed, you cannot edit pages in the wiki")
        }
        case Failure(msg, _, _) => error(msg)
        case _ => error("Not logged In")
      }
    }
    
    Can(RedirectResponse(from))
  }
  
  def logout(r: RequestState) = {
    val from = S.referer.openOr("/")
    
    User.logoutCurrentUser
    S.notice("Logged Out")
    
    Can(RedirectResponse(from))
  }
}
