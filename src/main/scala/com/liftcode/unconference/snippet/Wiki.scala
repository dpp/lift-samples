package com.liftcode.unconference.snippet

import com.liftcode.unconference._
import model._

import net.liftweb._
import http._
import S._
import util._
import Helpers._
import mapper._
import textile._

import scala.xml.{NodeSeq, Text, MetaData, Null}

class Wiki {
  
  def edit(in: NodeSeq): NodeSeq = 
  (for (user <- User.currentUser ?~ "Not logged in";
  cat <- S.param("category") ?~ "No category";
  page <- S.param("page") ?~ "No Page";
  val from = S.referer ?~ "Invalid Referrer";
  val actual = Entry.locate(cat, page)) yield {
    val revised = Entry.create.name(page).category(cat).author(user).
    text(actual.map(_.text.is).openOr(""))
    <div>
    <p>
    You are editting the {page} page in {cat}. {
      from.map(f => <a href={f}>Back</a>).openOr(Text(""))
    }
    </p>
    
    {
      S.textarea(revised.text,s => revised.text(s))
    }
    <br/>
    {
      S.submit("Save", s => {revised.save; redirectTo("/"+cat+"/"+urlEncode(page))})
    }
    </div>
  }) match {
    case Full(x) => x
    case Failure(msg, _, _) => error(msg); redirectTo("/")
    case _ => redirectTo("/")
  }
  
  def listAll: NodeSeq = (for (
  cat <- S.param("category") ?~ "No category"
  )
  yield {
    
    <div>
    {
      cat match {
        case Entry.News => <p>News and updates related to the Scala <i>lift</i> off</p>
        case Entry.Sessions => <p>Sessions that people are proposing for the Scala <i>lift</i> off</p>
        case Entry.Community => <p>Folks who have registered to attend the Scala <i>lift</i> off</p>
        case _ => Text("")
      }
    }
    {
      Entry.currentForCategory(cat).map(e => <a href={"/"+e.category+"/"+urlEncode(e.name)}>{e.name}</a><br/>)
    }
    </div>
  }) match {
    case Full(x) => x
    case Failure(msg, _, _) => S.error(msg); redirectTo("/")
    case _ => redirectTo("/")
  }
  
  private lazy val canEdit = User.currentUser.map(_.validated.is).openOr(false)
  
  private def writeUrl(category: String)(what: TextileParser.WikiURLInfo): (String, NodeSeq, Option[String]) = 
  what match {
    case TextileParser.WikiURLInfo(text, Some(cat)) if Entry.areas.contains(cat) =>
    ("/"+cat+"/"+urlEncode(text), Text(text), None)
    case TextileParser.WikiURLInfo(text, _) =>
    if (Entry.locate(category, text).isDefined) ("/"+category+"/"+urlEncode(text), Text(text), None)
    else ("/wki/"+urlEncode(text), Text(text), None)
  }
  
  def show: NodeSeq =  {
    (for (cat <- S.param("category") ?~ "No category";
    page <- S.param("page") ?~ "No Page";
    actual <- Entry.locate(cat, page)) yield {
      <div class="wiki_out">
      {
        TextileParser.toHtml(actual.text, Some(writeUrl(cat)))
      }
      <br/>
      <br/>
      {
        if (canEdit) <p><a href={"/"+cat+"/edit/"+urlEncode(page)}><button>edit</button></a></p>
        else Text("")
      }
      </div>
    }) match {
      case Full(x) => x
      case Failure(msg, _, _) => error(msg); redirectTo("/")
      case _ if User.currentUser.map(_.validated.is).openOr(false) => redirectTo("/"+S.param("category").open_! +
      "/edit/"+
      urlEncode(S.param("page").open_!))
      case _ => error("Page not found"); redirectTo("/")
    }
  }
}


