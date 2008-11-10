/*
 * ChatServer.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.liftweb.lib

import scala.actors._
import Actor._

object ChatServer extends Actor {
    private var msgs: List[String] = Nil
    private var listeners: List[Actor] = Nil

    def act = loop {
        react {
            case s: String =>
                msgs = s :: msgs
                listeners.foreach(_ ! Messages(msgs))

            case DoListen(who) =>
                listeners = who :: listeners
                who ! Messages(msgs)

            case DoUnlisten(who) =>
                listeners = listeners.filter(_ ne who)
        }
    }


    this.start()
}

case class Messages(msg: List[String])
case class DoListen(who: Actor)
case class DoUnlisten(who: Actor)