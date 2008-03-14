package com.liftcode.unconference.snippet

import com.liftcode._
import model._

import net.liftweb._
import http._
import S._
import util._
import mapper._

import scala.xml.{NodeSeq, Text}

class UnconfMenu {
  private def cmp(test: String, actual: String): Boolean = {
    if (test == "/" && actual == "/") true
    else if (test == "/") false
    else if (test == actual || actual.startsWith(test+"/")) true
    else false
  }
  
  
  private def clcCls(what: String, last: Boolean): Option[NodeSeq] =
  (what, S.uri, last) match {
    case (x , y, last) if cmp(x, y) => Some(Text("current" + (if (last) " last-link" else "")))
    case (_, _, true) => Some(Text("last-link"))
    case _ => None
  }
  
  def menu =
  <ul class="menu">
  <li><a href="/" class={clcCls("/", false)}><span>Home</span></a></li>
	<li><a href="/news" class={clcCls("/news", false)}><span>News</span></a></li>
  <li><a href="/sessions" class={clcCls("/sessions", false)}><span>Sessions</span></a></li>
  <li><a href="/feedback" class={clcCls("/feedback", false)}><span>Feedback</span></a></li>
  <li><a href="/community" class={clcCls("/community", true)}><span>Community</span></a></li>
  </ul>
}


