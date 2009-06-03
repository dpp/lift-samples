/*
 * Pong.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.fruit.comet

import net.liftweb._
import http._
import SHtml._
import js._
import JE._
import JsCmds._
import js.jquery._
import JqJE._
import util._
import Helpers._

import scala.actors._
import Actor._

object PongServer extends Actor {
  private var state = PongState.default
  private var listeners: List[Actor] = Nil
  def act = loop {
    react {
      case Up(0) =>
        state = state.leftUp
        updateListeners
      case Up(1) =>
        state = state.rightUp
        updateListeners

      case Down(0) =>
        state = state.leftDown
        updateListeners

      case Down(1) =>
        state = state.rightDown
        updateListeners

      case 'tick =>
        state = state.tick
        updateListeners
        reTick

      case AddAListener(who) =>
        val len = listeners.length
        listeners ::= who
        who ! state
        who ! len

      case RemoveAListener(who) =>
        listeners -= who

      case _ => 
    }
  }

  def reTick = ActorPing.schedule(this, 'tick, 500)

  def updateListeners = listeners.foreach(_ ! state)

  this.start
  reTick
}

case class Up(who: Int)
case class Down(who: Int)
case class PongState(left: Int, right: Int, ballTop: Int, ballLeft: Int, vel: (Int, Int), score: (Int, Int)) {
  def leftUp = PongState((left - 5) max 0, right, ballTop, ballLeft, vel, score)
  def leftDown = PongState((left + 5) min 100, right, ballTop, ballLeft, vel, score)
  def rightUp = PongState(left, (right - 5) max 0, ballTop, ballLeft, vel, score)
  def rightDown = PongState(left, (right + 5) min 100, ballTop, ballLeft, vel, score)
  def tick = (ballTop + vel._1, ballLeft + vel._2 ) match {
    case (top, lp) if lp < 0 =>
      val off = top - left
      if (Math.abs(off) <= 9) PongState(left, right, top, ballLeft, (vel._1 + off, -vel._2), score)
      else PongState(left, right, 50, 50, (0, 5), (score._1, score._2 + 1))

      case (top, lp) if lp > 99 =>
      val off = top - right
      if (Math.abs(off) <= 9) PongState(left, right, top, ballLeft, (vel._1 + off, -vel._2), score)
      else PongState(left, right, 50, 50, (0, 5), (score._1 + 1, score._2))

    case (top, lp) if top < 0 | top > 99 => PongState(left, right, ballTop, lp, (-vel._1, vel._2), score)
    case (top, lp) => PongState(left, right, top, lp, vel, score)
  }
}

object PongState {
  def default = PongState(50, 50, 50, 50, (0, 5), (0,0))
}

class Pong extends CometActor with CometListener {
  private var state = PongState.default
  private var myNum = 0

  appendJsonHandler {
    case JsonCmd("up", _, _, _) =>
      PongServer ! Up(myNum)
      Noop
      
    case JsonCmd("down", _, _, _) =>
      PongServer ! Down(myNum)
      Noop 
  }

  def registerWith = PongServer

  override def highPriority = {
    case i: Int => myNum = i

    case s: PongState => state = s; partialUpdate(drawIt)
  }

  def drawIt = (JqId("ball") >> JqCss("top", state.ballTop * 3 - 5) >> JqCss("left", state.ballLeft * 4 - 5)) &
  (JqId("left") >> JqCss("top", state.left * 3 - 25) ) &
  (JqId("right") >> JqCss("top", state.right * 3 - 25) )

  def render =
  <xml:group>
    <div id="board" style="width: 400px; height: 300px; position: relative; background-color: black">
      <div id="ball" style="width: 10px; height: 10px; position: absolute; background-color: white"/>
      <div id="left" style="left: 1px; width: 10px; height: 50px; position: absolute; background-color: white"/>
      <div id="right" style="left: 389px; width: 10px; height: 50px; position: absolute; background-color: white"/>
    </div>
    {
      Script(OnLoad(JqDoc >> JqKeypress(
            'k' -> jsonCall("up"),
              'm' -> jsonCall("down"))) &
             OnLoad(drawIt))
    }
  </xml:group>

}
