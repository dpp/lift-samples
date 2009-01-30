package me.uplifting.comet
/**
 * Created by IntelliJ IDEA.
 * User: dpp
 * Date: Jan 28, 2009
 * Time: 8:23:52 PM
 * To change this template use File | Settings | File Templates.
 */

import net.liftweb._
import http._

import scala.xml._

trait ToeDelta extends DeltaTrait

object ToeStyle {
  val style =
  """
.xo {
  font-size: 64pt;
}
.vspace {
  padding: 0px;
  margin: 0px;
  border: 0px;
  background: #000;
}
.hspace {
  padding: 0px;
  margin: 0px;
  border: 0px;
  background: #000;
}
.sspace {
  padding: 0px;
  margin: 0px;
  border: 0px;
  background: #000;
}
"""
}

case class Player(name: String) {
  lazy val id = 
}

class ToeBoard(p1: String, p2: String) extends CometState[ToeDelta, ToeBoard] {
  private var info: List[Option[Boolean]] = Nil

  private def b(pos: Int): Node = info(pos) match {
    case Some(true) => Text("X")
    case Some(false) => Text("O")
    case _ => <xml:group>&nbsp;</xml:group>
  }

  lazy val winner: Option[Boolean] = None

  lazy val nextPlayer: Option[Boolean] =
  if (winner.isDefined) None
  else {
    Some((info.foldLeft(0)(
          (a, b) => a + (if(b.isDefined) 1 else 0))) % 2 == 0)
  }



  def -(other: ToeBoard): Seq[ToeDelta]
  def render: NodeSeq =
  <span>
    <style>
      {Unparsed("/* <![CDATA[ */")}
      {Unparsed(ToeStyle.style)}
      {Unparsed("/* ]]> */")}
    </style>
    <table cellpadding="0" cellspacing="0">
      <tr>
        <td class="xo"><div id="B0">{b(0)}</div></td>
        <td class="vspace">&nbsp;</td>
        <td class="xo"><div id="B1">{b(1)}</div></td>
        <td class="vspace">&nbsp;</td>
        <td class="xo"><div id="B2">{b(2)}</div></td>
      </tr>

      <tr>
        <td class="hspace">&nbsp;</td>
        <td class="sspace">&nbsp;</td>
        <td class="hspace">&nbsp;</td>
        <td class="sspace">&nbsp;</td>
        <td class="hspace">&nbsp;</td>
      </tr>
      <tr>
        <td class="xo"><div id="B3">{b(3)}</div></td>
        <td class="vspace">&nbsp;</td>
        <td class="xo"><div id="B4">{b(4)}</div></td>
        <td class="vspace">&nbsp;</td>
        <td class="xo"><div id="B5">{b(5)}</div></td>
      </tr>

      <tr>
        <td class="hspace">&nbsp;</td>
        <td class="sspace">&nbsp;</td>
        <td class="hspace">&nbsp;</td>
        <td class="sspace">&nbsp;</td>
        <td class="hspace">&nbsp;</td>
      </tr>
      <tr>
        <td class="xo"><div id="B6">{b(6)}</div></td>
        <td class="vspace">&nbsp;</td>
        <td class="xo"><div id="B7">{b(7)}</div></td>
        <td class="vspace">&nbsp;</td>
        <td class="xo"><div id="B8">{b(8)}</div></td>
      </tr>
    </table>
  </span>
}