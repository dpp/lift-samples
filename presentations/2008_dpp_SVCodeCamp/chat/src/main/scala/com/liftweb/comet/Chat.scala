/*
 * Chat.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.liftweb.comet

import com.liftweb.lib._
import net.liftweb._
import http._
import SHtml._

class Chat extends CometActor {
    private var msgs: List[String] = Nil

    def defaultPrefix = "na"

    def render =
    <xml:group>
        <ul>
            {
                msgs.take(40).reverse.map(m => <li>{m}</li>)
            }
        </ul>
         {
             ajaxForm(
                 text("", ChatServer ! _) ++
                 <input type="submit" value="chat" />
             )
         }
    </xml:group>

    override def localSetup() {
        ChatServer ! DoListen(this)
        super.localSetup()
    }

    override def localShutdown() {
        ChatServer ! DoUnlisten(this)
        super.localShutdown()
    }

    override def lowPriority = {
        case Messages(m) =>
            msgs = m
            reRender(false)
    }
}
