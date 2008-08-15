package com.skittr.actor

import scala.actors._
import scala.actors.Actor._


class UserActor(val guid: GUID, backingStore: BackingStore) extends TransActor {
  // private var 
  def act = loop {
    react {
      case _ =>
    }
  }
  
  def moo: Unit = {
    guid.! ("Hello") ( {
      case _ => ()
    })
    
  }
}

trait GUID {
  def toLong: Long
  def !(msg: Any)(reply: PartialFunction[Any, Unit]): Unit
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

/*
trait Key {
     def send(msg: Any, responseFunc: Function[Any, Unit],
timeoutAndRetryRules: TimeoutAndRetryRules,
     failure: Function[(FailReason, Target, Msg, StateThingy)])

     def ~>(msg: Any) // send with no response
     def defaultRetryRules: TimeoutAndRetryRules

     def ~?>(msg: Any)(responseFunc: Function[Any, Unit])

     def doWithFailureRules[T](f: => T)(....)
}

class User extends Actor with Key {
 val followers: List[Key]
 val devices: List[Key with DeviceMagic]
 var updates: List[Updates]

 def defaultRetryRules = {
   case Timeout(elapsedSinceLastTry, totalElapsedTime, retryCount) =>
   case IOError(....) =>
   case ...
 }

 def act {
   loop {
     react {
       case TimelineUpdate(update) =>
       doWithFailureRules {
         updates = update :: updates // need idempotency here
         appendUpdateToBackingStoreIfNotDup(update)
         followers.foreach(_ ~> update)
         globalTimelineService ~> update
         for (dev <- devices if dev.validFor(update)) dev ~> update
         magicSender.respondWith(NewTimeline(updates.take(20)))
       } onFailure { // might get here more than once
         ...         // but don't get here until you've finished
processing the TimelineUpdate message
       }

       case TimelineUpdate(update) =>
       doWithFailureRules {
         updates = update :: updates
         followers.foreach(_ ~> update)
       } onFailure { // might get here more than once
       }
       doWithFailureRules {
         globalTimelineService ~> update
       } onFailure { // might get here more than once
       }
       doWithFailureRules {
         for (dev <- devices if dev.validFor(update)) dev ~> update
       } onFailure { // might get here more than once
       }
       doWithFailureRules {
         magicSender.respondWith(NewTimeline(updates.take(20)))
       } onFailure { // might get here more than once
       }

       case GetFriendsTimeline(otherGuy) =>
       val cachedMagicSender = magicSender
       doWithFailureRules {
         otherGuy ~?> GetFollowers {
           // if any of the timeline entries are posted by somebody
who is protected...
           // only show them if we're following the same friends.
           //
           case Followers(followers) =>
             val listOfVisibleUpdates =
filterUpdatesForMeAndTheFollowers(followers)
             MasterUpdateServer ~?> GetTextOfUpdates(listOfVisibleUpdaes) {
               case TextOfUpdates(updates) =>
               cachedMagicSender.respondWith(TheRealizedUpdates(updates))
             }
         }
       } onFailure {
         case (failureReason, _) =>
cachedMagicSender.respondWith(FailedToFetchUpdates)
       }
     }
   }
 }
}

// -----------------------------------------

 val user: Key = ...
 user ~?> TimelineUpdate(..msg..) {
   case NewTimeline(timeline) => ... display ...
 }

 user ~?> GetFriendsTimeline(...me...) {
   case TheRealiedUpdates(...) =>
   case Fai
*/
