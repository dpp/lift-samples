package com.liftcode.unconference.snippet

import net.liftweb._
import http._
import util._
import Helpers._

import com.liftcode.unconference._
import model._

import scala.xml.Text

class Search {
  def render = S.param("what").map {
    what =>
    <p>Search results for {what}:
    <ul>
    {
      for (e <- Entry.search(what)) yield <li><a href={"/"+e.category+"/"+urlEncode(e.name)}>{e.category+":"+e.name}</a></li>
    }
    </ul>
    </p>
  } openOr Text("No Search Term")
}

