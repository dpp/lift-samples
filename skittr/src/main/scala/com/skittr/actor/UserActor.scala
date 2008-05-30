package com.skittr.actor

import scala.actors._
import scala.actors.Actor._


class UserActor(val guid: GUID, backingStore: BackingStore) extends TransActor {
  // private var 
  def act = loop {
    guid ! "Hello"
    react {
      
      case _ =>
    }
  }
}

trait GUID {
  def toLong: Long
  def !(msg: Any): Unit
}


trait TransActor extends Actor {
  def guid: GUID
  
  override def react(f: PartialFunction[Any, Unit]): Nothing = {
    super.react(f)
  }
}


trait StatusInfo {
  def poster: GUID
  def time: Long
  def protected_? : Boolean
  def statusId: GUID
  def toLong: (Long, Long, Long, Long)
}

trait BackingStore {
  def mostRecentTimeline(func: StatusInfo => Boolean): Unit
  def mostRecentFollowing(func: StatusInfo => Boolean): Unit
  def appendToTimeline(stat: StatusInfo): Unit
  def appendToFollowing(stat: StatusInfo): Unit
  
  def followers(): List[GUID]
  def following(): List[GUID]
  
  def followers(f: List[GUID])
  def following(f: List[GUID])
  
  def removeFromTimeline(stat: StatusInfo): Unit
  def removeFromFollowing(stat: StatusInfo): Unit
  
  def truncateFollowingBefore(when: Long): Unit
  
  def userInfo(): UserInfo
  def userInfo(info: UserInfo): Unit
}

trait UserInfo {
  def name: String
  def bio: String
  def protected_? : Boolean
  def url: String
  def handle: String
}
