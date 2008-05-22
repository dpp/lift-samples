package com.liftcode.unconference.model

import net.liftweb.mapper._
import net.liftweb.util._
import Helpers._
import java.util.regex.Pattern

/**
* The Entry MetaObject
*/
object Entry extends Entry with KeyedMetaMapper[Long, Entry] {
  @transient
  private var entries: Map[(String, String), Can[Entry]] = Map.empty
  
  private def addToEntries(e: Entry) {
    val key = (e.category.is, e.name.is)
    
    entries += (key -> Full(e))
  }
  
  def locate(category: String, name: String): Can[Entry] = {
    val key = (category, name)
    entries.get(key) match {
      case Some(ret) => ret
      case _ => val ret = find(By(Entry.name, name), By(Entry.category, category),
      MaxRows(1), OrderBy(createdAt, false))
      entries += (key -> ret)
      ret
    }
  }
  
  private def buildMatcher(in: String): String => Boolean  = {
    val lower = in.toLowerCase
    try {
      val pat = Pattern.compile(in)
      s => {
        if (s eq null) false else {
        val m = pat.matcher(s)
        m.find() || s.toLowerCase.indexOf(lower) >= 0
        }
      }
    } catch {
      case _ => s => (s ne null) && s.toLowerCase.indexOf(lower) >= 0
    }
  }
  
  def search(regex: String): List[Entry] = {
    val matcher = buildMatcher(regex)
    val ret = for (cat <- areas;
    e <- currentForCategory(cat) if matcher(e.name+"\n"+e.text)) yield e
    
    ret.sort(_.createdAt.is > _.createdAt.is)
  }
  
  def currentForCategory(category: String): List[Entry] = {
    DB.runQuery("SELECT DISTINCT name FROM entry_t WHERE category = ?", List(category))._2.
    flatMap(h => locate(category, h.head)).sort(_.createdAt.is > _.createdAt.is)
  }
  
  override def afterSave = addToEntries _ :: super.afterSave
  
  
  val News = "news"
  val Sessions = "sessions"
  val Community = "community"
  val Wiki = "wiki"
  val Home = "home"
  val areas = List(News, Sessions, Community, Wiki, Home)
  
  def editUrl(cat: String, name: String): String = "/"+cat+"/edit/"+urlEncode(name)
}

/**
* A wiki entry
*/
class Entry extends KeyedMapper[Long, Entry] {
  def getSingleton = Entry // what's the "meta" server
  def primaryKeyField = id
  
  object id extends MappedLongIndex(this)
  
  object createdAt extends MappedLong(this) {
    override def defaultValue = millis
  }
  
  object name extends MappedString(this, 255) {
    override def setFilter = notNull _ :: trim _ :: super.setFilter
    override def dbIndexed_? = true
  }
  
  object category extends MappedString(this, 255)
  
  object text extends MappedFakeClob(this) {
    override def defaultValue ="*nothing*"
  }
  
  object author extends MappedLongForeignKey(this, User)
  
  def editUrl: String = Entry.editUrl(category, name)
}
