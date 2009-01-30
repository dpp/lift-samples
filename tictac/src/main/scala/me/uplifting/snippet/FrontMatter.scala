/*
 * FrontMatter.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package me.uplifting.snippet

import net.liftweb._
import http._
import util._
import SHtml._

import scala.actors.Actor
import Actor._

object MyId extends SessionVar(Helpers.nextFuncName)

object MyName extends SessionVar[Box[String]](Empty) {
  registerCleanupFunc(() => LobbyServer ! RemoveLurker(MyId))
}

object CurrentGameActor extends SessionVar[Box[CometActor]](Empty)

class FrontMatter {
  def render = (MyName.is, CurrentGameActor.is) match {
    case (_, Full(_)) => <lift:comet type="TicTacGame" />
    case (Full(_), _) => <lift:comet type="Lobby" />
    case _ => <lift:embed what="/templates-hidden/ask_name"/>
  }

  def getName =
  <div>
    Please enter your name: {
      text("", testAndSetName)
    } <br/>
    <input type="submit" value="Set Name"/>
  </div>

  private def testAndSetName(n: String) {
    if (n.trim.length > 1) {
      MyName(Full(n.trim))
      LobbyServer ! AddLurker(MyId, n.trim)
    }
  }
}

case class RemoveLurker(who: String)
case class AddLurker(who: String, name: String)
case class Add(who: Actor)
case class Remove(who: Actor)
case class Lurkers(x: List[(String, String)])

object LobbyServer extends Actor {
  private var here: List[(String, String)] = Nil
  private var listeners: List[Actor] = Nil
  this.start

  private def updateAll = listeners.foreach(_ ! Lurkers(here))

  def act = loop {
    react {
      case AddLurker(id, name) =>
        here ::= (id, name)
        updateAll

        case RemoveLurker(id) =>
          here = here.remove(_._1 == id)
          updateAll

        case Add(who) =>
          listeners ::= who
          who ! Lurkers(here)

        case Remove(who) =>
          listeners -= who
    }
  }
}