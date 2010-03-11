package me.uplifting.lib
/**
 * Created by IntelliJ IDEA.
 * User: dpp
 * Date: Jan 28, 2009
 * Time: 8:23:52 PM
 * To change this template use File | Settings | File Templates.
 */

import net.liftweb._
import common._
import http._
import SHtml._
import js._
import JsCmds._
import util._
import js._
import JsCmds._
import JE._

import net.liftweb.http.jquery._
import net.liftweb.http.js.jquery._
import JqJE._

import scala.xml._
import net.liftweb.actor._

import snippet._

trait ToeDelta extends DeltaTrait

object ToeStyle {
  val style =
  """

"""
}

case class Player(name: String) {
  val id = Helpers.nextFuncName
}

object ToeBoard {
  def Empty = new ToeBoard()

  val winners = List(
    List(0, 1, 2),
      List(3,4,5),
      List(6,7,8),
      List(0, 3, 6),
      List(1,4,7),
      List(2, 5, 8),
      List(0,4,8),
      List(2,4,6))
}

class ToeBoard extends CometState[ToeDelta, ToeBoard] {
  private var player1: Box[Player] = Empty
  private var player2: Box[Player] = Empty
  private var info: Array[Box[Boolean]] = {
    val ret = new Array[Box[Boolean]](9)
    (0 until 9).foreach(i => ret(i) = None)
    ret
  }

  private def mkHtml(in: Box[Boolean]) = in match {
    case Full(true) => Text("X")
    case Full(false) => Text("O")
    case _ => <xml:group>&nbsp;</xml:group>
  }

  private def b(pos: Int): Node = mkHtml(info(pos))

  private def d(pos: Int): NodeSeq = info(pos) match {
    case Full(_) => Text("no")
    case _ => Text("yes")
  }

  private def removeClicks = (0 to 8).map(i =>
    JsRaw("document.getElementById('B"+i+"').onclick = null;")).foldLeft(Noop)(_ & _)

  private def loadScript(): JsCmd = if (nextPlayer == MyName.is) {
    OnLoad(removeClicks &
           info.toList.zipWithIndex.filter(_._1.isEmpty).
           map(_._2).map(i => JsRaw("document.getElementById('B"+i+"').onclick = doClick;")).foldLeft(Noop)(_ & _))
  } else OnLoad(removeClicks)

  private def currentMessage(): NodeSeq = {
    if (nextPlayer == MyName.is) {
      <b>{MyName.is.map(_.name) openOr ""} it's your turn</b>
    } else {
      if (gameOver) {
        <xml:group>
          {
            winner match {
              case Full(p) => <span>{p.name} Wins!!</span>
              case _ => <span>Tie Game</span>
            }
          }
          {
            ajaxButton("Back to the Lobby",
                       () => {
                println("Current board is "+CurrentTicTacToeGameActor.is+" user "+MyName)

                CurrentTicTacToeGameActor.remove

                println("After Current board is "+CurrentTicTacToeGameActor.is+" user "+MyName)

                CurrentCometActor.value.foreach(_ ! ShutDown)
                              
                              RedirectTo("/")})
          }
        
        </xml:group>
      } else {
        <span>Please wait for the other player to make a move</span>
      }
    }
  }
  /*
   ajaxButton("Back To Lobby",
   () => {CurrentCometActor.value.foreach(_ ! ShutDown); RedirectTo("/")}))
   else Noop
   */
  private def isWinner(test: List[Int]): Boolean =
  test.map(info).reduceLeft((a, b) => if (a == b) a else None).isDefined

  lazy val winner: Box[Player] =
  ToeBoard.winners.find(isWinner).flatMap(i => info(i.head)).flatMap {
    case true => player1
    case false => player2
  }

  private lazy val gameOver = winner.isDefined ||
  (info.filter(_.isEmpty).length == 0)

  private lazy val nextPlayerBool: Box[Boolean] =
  if (gameOver) None else
  Some((info.foldLeft(0)(
        (a, b) => a + (if(b.isDefined) 1 else 0))) % 2 == 0)

  lazy val nextPlayer: Box[Player] = 
  nextPlayerBool.flatMap {
    case true => player1
    case _ => player2
  }

  private def myTurn = !gameOver && nextPlayer.isDefined &&
  nextPlayer == MyName.is

  def mark(cell: Int): ToeBoard = if (cell < 0 || cell >= info.length) this
  else if (info(cell).isDefined) this
  else {
    val ret = new ToeBoard
    ret.player1 = player1
    ret.player2 = player2
    System.arraycopy(info, 0, ret.info, 0, info.length)
    ret.info(cell) = nextPlayerBool
    ret
  }

  def addPlayer(p: Player): ToeBoard = {
    val ret = new ToeBoard
    ret.info = info
    ret.player1 = player1
    ret.player2 = player2
    player1 match {
      case Empty => ret.player1 = Full(p)
      case _ => ret.player2 = Full(p)
    }
    ret
  }

  def -(other: ToeBoard): Seq[ToeDelta] =
  UpdateMessage() ::
  info.zipWithIndex.zip(other.info).flatMap {
    case ((me, idx), them) if me != them => List((me, idx))
    case _ => None
  }.map(makeCellChanger).toList

  private case class CellChanger(what: Box[Boolean],pos: Int) extends ToeDelta {
    def toJs: JsCmd = SetHtml("B"+pos, mkHtml(what))
  }

  private case class UpdateMessage() extends ToeDelta {
    def toJs = SetHtml("ttt_msg", currentMessage()) & loadScript()
                                     
  }

  private def makeCellChanger(in: (Box[Boolean], Int)) = CellChanger(in._1, in._2)

  def render: NodeSeq =
  <span id="ttt_board">
    {
      Script(Function("doClick", List("what"), 
                      CurrentCometActor.value.map(
            _.jsonCall("change", JsRaw("what.target.id"))) openOr
                      Noop))
    }
   
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
    <div id="ttt_msg">{currentMessage()}{Script(loadScript())}</div>
  </span>
}

object CurrentTicTacToeGameActor extends SessionVar[Box[TicTacToeGameActor]](Empty)

class TicTacToeGameActor extends LiftActor {
  private var listeners: List[LiftActor] = Nil
  private var currentGame = ToeBoard.Empty

  private def updateListeners() {
    listeners.foreach(_ ! currentGame)
  }

  def messageHandler =
    {
      case Add(who) => listeners ::= who ; who ! currentGame
      case Remove(who) => listeners -= who

      case AddPlayer(who) =>
        currentGame = currentGame.addPlayer(who)
        updateListeners()

      case MarkCell(cell) => currentGame = currentGame.mark(cell)
        updateListeners()
    }
}

case class Add(who: LiftActor)
case class Remove(who: LiftActor)
case class AddPlayer(who: Player)
case class MarkCell(cell: Int)
