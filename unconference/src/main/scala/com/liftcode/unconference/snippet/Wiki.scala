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
import textile._

import scala.xml.{NodeSeq, Text, MetaData, Null}

class Wiki {
  
  def edit(in: NodeSeq): NodeSeq = 
  (for (user <- User.currentUser ?~ "Not logged in";
  cat <- S.param("category") ?~ "No category";
  page <- S.param("page") ?~ "No Page";
  val from = S.referer ?~ "Invalid Referrer";
  val actual = entry(cat, page, S.param("id"))) yield {
    val revised = Entry.create.name(page).category(cat).author(user).
    text(actual.map(_.text.is).openOr(""))
    <div>
    <p>
    You are editting the {page} page in {cat}. {
      from.map(f => <a href={f}>Back</a>).openOr(Text(""))
    }
    </p>
    
    {
      SHtml.textarea(revised.text,s => revised.text(s))
    }
    <br/>
    {
      SHtml.submit("Save", () => {revised.save; redirectTo("/"+cat+"/"+urlEncode(page))})
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
        case Entry.News => 
        <p>News and updates related to the Scala <i>lift</i> off {
          if (canEdit)
          <span>&nbsp;Add: <form style="display: inline" action="/news/add"><input name="name" type="text"/><input type="submit" value="New"/></form>
          </span>
          else Text("")
        }</p> 
        case Entry.Sessions => <p>Sessions that people are proposing for the Scala <i>lift</i> off{
          if (canEdit)
          <span>&nbsp;Add: <form style="display: inline" action="/sessions/add"><input name="name" type="text"/><input type="submit" value="New"/></form>
          </span>
          else Text("")
        }</p>
        case Entry.Community => <p>Folks who have registered to attend the Scala <i>lift</i> off</p>
        case _ => Text("")
      }
    }
    {
      Entry.currentForCategory(cat).map(e => <xml:group><a href={"/"+e.category+"/"+urlEncode(e.name)}>{e.name}</a> &nbsp;<span style="font-size: 0.8em"><a href={"/"+e.category+"/history/"+urlEncode(e.name)}>History</a></span><br/></xml:group>)
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
    ("/"+cat+"/"+urlEncode(text), Text(text), if (Entry.locate(cat, text).isDefined) None else
    Some("need_edit"))
    case TextileParser.WikiURLInfo(text, _) =>
    ("/"+category+"/"+urlEncode(text), Text(text), if (Entry.locate(category, text).isDefined) None else
    Some("need_edit"))
  }
  
  private def entry(cat: String, name: String, id: Box[String]): Box[Entry] = 
  id match {
    case Full(id) =>
    Entry.find(id).flatMap{
      case e if e.category.is == cat && e.name.is == name => Full(e)
      case _ => Empty
    }
    case _ => Entry.locate(cat, name)
  }
  
  def show: NodeSeq =  {
    (for (cat <- S.param("category") ?~ "No category";
    page <- S.param("page") ?~ "No Page";
    val actual = entry(cat, page, S.param("id"))) yield {
      actual match {
        case Full(actual) =>
      <div class="wiki_out">
      {
        TextileParser.toHtml(actual.text, Some(writeUrl(cat) _))
      }
      <br/>
      <br/>
      {
        if (canEdit) <p><form action={actual.editUrl}><input type="submit" value="Edit"/></form></p>
        else Text("")
      }
      <a href={"/"+cat+"/history/"+urlEncode(page)}>History</a>
      </div>
      case _ if User.currentUser.map(_.validated.is).openOr(false) => redirectTo(Entry.editUrl(cat, page))
      case _ => error("Page not found"); redirectTo("/")
      }
    }) match {
      case Full(x) => x
      case Failure(msg, _, _) => error(msg); redirectTo("/")
      case _ => error("Page not found"); redirectTo("/")
    }
  }
  
  def history: NodeSeq = {
    (for (cat <- S.param("category") ?~ "No category";
    page <- S.param("page") ?~ "No Page";
    val actual = Entry.findAll(By(Entry.category, cat), By(Entry.name, page), OrderBy(Entry.createdAt, Descending))) yield {
      actual match {
        case Nil => <b>No entries found for {page} in {cat}</b>
        case xs =>
        <p>
        History for {page} in {cat}
        <ul>
        {
          xs.map(e => <li><a href={"/"+cat+"/"+urlEncode(page)+"?id="+e.id}>{new java.util.Date(e.createdAt.is).toString}</a> by 
          <a href={"/community/"+urlEncode(e.author.obj.map(_.wikiName.is).openOr(""))}>{e.author.obj.map(_.niceName).openOr("")}</a></li>)
        }
        </ul>
        </p>
      }
    }) match {
      case Full(x) => x
      case Failure(msg, _, _) => error(msg); redirectTo("/")
      case _ => error("Page not found"); redirectTo("/")
    }
  }
}


