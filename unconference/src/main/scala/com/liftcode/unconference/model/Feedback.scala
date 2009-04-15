package com.liftcode.unconference.model

import net.liftweb.mapper._
import net.liftweb.util._
import Helpers._
import java.util.regex.Pattern

import Mailer._


/**
* The Entry MetaObject
*/
object Feedback extends Feedback with KeyedMetaMapper[Long, Feedback] {
  override def afterCreate = fireEmail _ :: super.afterCreate
  
  def fireEmail(what: Feedback) {
    Mailer.sendMail(From("feedback@scalaliftoff.com"),Subject("[SlO] Feedback"), 
    ReplyTo(what.email), BCC("feeder.of.the.bears@gmail.com"), To("feedback@scalaliftoff.com"),
    PlainPlusBodyType(what.text, "UTF-8"))
  }
}

/**
* Feedback
*/
class Feedback extends KeyedMapper[Long, Feedback] {
  def getSingleton = Feedback // what's the "meta" server
  def primaryKeyField = id
  
  object id extends MappedLongIndex(this)
  
  object createdAt extends MappedLong(this) {
    override def defaultValue = millis
  }
  
  object text extends MappedString(this, 8192) {
    override def defaultValue =""
    
    override def validations = valMinLen(20, "Message too short") _ :: super.validations
    
    override def setFilter = crop _ :: super.setFilter
  }
  
  object email extends MappedEmail(this, 64)
  
  object author extends MappedLongForeignKey(this, User)
  
  object responseTo extends MappedLongForeignKey(this, Feedback)
}
