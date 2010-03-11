/*
 * Lobby.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package me.uplifting.comet

import net.liftweb._
import http._
import js._
import JsCmds._
import util._
import common._

import lib._
import snippet._

class Lobby extends CometActor {
  private var inLobby: List[Player] = Nil
  
  override def localSetup() {
    MyName.is.foreach(me => 
      LobbyServer ! AddLurker(me, this))
  }
  
  override def localShutdown() {
    MyName.is.foreach(me => 
      LobbyServer ! RemoveLurker(me))
  }
  
  def render =
  <div>
    <p>We're waiting until someone else shows up to play Tic Tac Toe</p>
    <ul>
      {
        inLobby.map(p => <li>{p.name}</li>)
      }
    </ul>
  </div>

  override def lowPriority = {
    case Lurkers(lurk) => inLobby = lurk
      reRender(false)

    case PlayGame(game) =>
      CurrentTicTacToeGameActor(Full(game))
      partialUpdate(RedirectTo("/"))
      this ! ShutDown
  }
}
