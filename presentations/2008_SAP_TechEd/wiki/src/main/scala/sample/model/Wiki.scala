/*
 * Wiki.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */
package sample.model


import net.liftweb.mapper._

object Wiki extends Wiki with LongKeyedMetaMapper[Wiki]

class Wiki extends LongKeyedMapper[Wiki] {
  def getSingleton = Wiki // what's the "meta" object
  def primaryKeyField = id

  // the primary key
  object id extends MappedLongIndex(this)
  
  // the name of the entry
  object name extends MappedString(this, 32) {
    override def dbIndexed_? = true // indexed in the DB
  }
  
  object owner extends MappedLongForeignKey(this, User)
  
  // the text of the entry
  object entry extends MappedTextarea(this, 8192)
}
