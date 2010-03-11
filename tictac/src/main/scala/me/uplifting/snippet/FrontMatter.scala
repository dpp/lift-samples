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
import common._
import SHtml._

import lib._

import net.liftweb.actor._

object MyName extends SessionVar[Box[Player]](Empty) {
  override def onShutdown(session: LiftSession) {
    is.foreach(who => LobbyServer ! RemoveLurker(who))
  }
}

class FrontMatter {
  def render = 
  (MyName.is, CurrentTicTacToeGameActor.is) match {
    case (_, Full(_)) => <lift:comet type="ToeDisplay" />
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
      val p = Player(n.trim)
      MyName(Full(p))
    }
  }
}

case class RemoveLurker(who: Player)
case class AddLurker(who: Player, actor: LiftActor)
case class Lurkers(x: List[Player])
case class PlayGame(server: TicTacToeGameActor)

object LobbyServer extends LiftActor {
  private var here: List[(Player, LiftActor)] = Nil

  private def updateAll = {
    val msg = Lurkers(here.map(_._1))
    here.foreach(_._2 ! msg)
  }

  def messageHandler = 
    {
      case AddLurker(p, who) =>
        here ::= p -> who
      here = here match {
        case (_, a1) :: (_, a2) :: rest =>
          val ga = new TicTacToeGameActor
        a1 ! PlayGame(ga)
        a2 ! PlayGame(ga)
        rest
        case xs => xs
      }
      updateAll
      
      case RemoveLurker(p) =>
        here.remove(_._1 == p)
      updateAll
      }
}
