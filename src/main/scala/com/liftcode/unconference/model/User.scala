package com.liftcode.unconference.model

import net.liftweb.mapper._
import net.liftweb.util._

/**
* The singleton that has methods for accessing the database
*/
object User extends User with MetaMegaProtoUser[User, User with KeyedMetaMapper[Long, User]] {
  override def dbTableName = "users" // define the DB table name
  
}

/**
* An O-R mapped "User" class that includes first name, last name, password and we add a "Personal Essay" to it
*/
class User extends MegaProtoUser[User] {
  def getSingleton = User // what's the "meta" server
  def primaryKeyField = id
  
  object paypalId extends MappedUniqueId(this, 20) {
    override def dbIndexed_? = true
  }
  
  object wikiName extends MappedString(this, 255)
  
  def makeWikiName(): User = {
    var norm = camelize(firstName, lastName)
    while (User.find(By(User.wikiName, norm)).isDefined) {
      norm = norm + "Another"
    }
    wikiName(norm)
    this.save
    
    Entry.locate(Entry.Community, norm) match {
      case Empty => Entry.create.name(norm).category(Entry.Community).author(this).save
      case _ =>
    }
    
    this
  }
  
  private def camelize(fn: String, ln: String) = (camWord(fn.toList), camWord(ln.toList)) match {
    case ("", "") => "FirstLast"
    case ("", last) => "First"+last
    case (first, "") => first+"nLast"
    case (first, last) => first+last
  }
  
  private def fixWord(in: List[Char]): List[Char] = in match {
    case Nil => Nil
    case c :: rest if c.isLetter || c.isDigit => c.toLowerCase :: fixWord(rest)
    case c :: rest => fixWord(rest)
  }
  
  private def camWord(in: List[Char]): String = in match {
    case Nil => ""
    
    case c :: rest if c.isLetter => val ret = c.toUpperCase + fixWord(rest).mkString
    if (ret.length == 1) ret + "n"
    else ret
    
    case c :: rest => camWord(rest)
  }
  
  def invalid_? = false
  def invalidReason = ""
}
