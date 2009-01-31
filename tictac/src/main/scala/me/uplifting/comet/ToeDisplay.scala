/*
 * ToeBoard.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package me.uplifting.comet

import net.liftweb._
import http._
import util._
import Helpers._
import js._
import JsCmds._

import lib._
import snippet._

class ToeDisplay extends StatefulComet {
  type Delta = ToeDelta
  type State = ToeBoard

  def emptyState = ToeBoard.Empty

  private val toeActor = CurrentTicTacToeGameActor.is.open_!
  private val me = MyName.is.open_!

  override def handleJson(in: Any): JsCmd = in match {
    case JsonCmd("change", _ , id: String, _) =>
      tryo{
        toeActor ! MarkCell(id.substring(1).toInt)
      }
      Noop
    case _ => Noop
  }

  /**
   * Test the parameter to see if it's an updated state object
   */
  def testState(in: Any): Box[State] = in match {
    case c: ToeBoard => Full(c)
    case _ => Empty
  }

  override def localSetup() {
    toeActor ! Add(this)
    toeActor ! AddPlayer(me)
  }

  override def localShutdown() {
    CurrentTicTacToeGameActor(Empty)
    toeActor ! Remove(this)
  }
}
