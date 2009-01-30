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

import lib._

class ToeDisplay extends StatefulComet {
  type Delta = ToeDelta
  type State = ToeBoard

  def emptyState = ToeBoard.Empty

  /**
   * Test the parameter to see if it's an updated state object
   */
  def testState(in: Any): Box[State] = in match {
    case c: ToeBoard => Full(c)
    case _ => Empty
  }

}
