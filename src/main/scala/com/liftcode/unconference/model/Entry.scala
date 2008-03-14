package com.liftcode.unconference.model

import net.liftweb.mapper._
import net.liftweb.util._
import Helpers._

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
  
  def currentForCategory(category: String): List[Entry] = {
  DB.runQuery("SELECT DISTINCT name FROM entry_t WHERE category = ?", List(category))._2.
  flatMap(h => locate(category, h.head)).sort(_.createdAt.is > _.createdAt.is)
  }
  
  override def afterSave = addToEntries _ :: super.afterSave
  
  
  val News = "news"
  val Sessions = "sessions"
  val Community = "community"
  val Wiki = "wki"
  val Home = "home"
  val areas = List(News, Sessions, Community, Wiki, Home)
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

  object text extends MappedFakeClob(this)

  object author extends MappedLongForeignKey(this, User)

  
}
