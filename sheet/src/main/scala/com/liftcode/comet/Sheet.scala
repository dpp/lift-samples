package com.liftcode.comet

import scala.actors._
import scala.actors.Actor._
import net.liftweb.http._
import net.liftweb.http.js._
import net.liftweb.http.js.JsCmds._
import net.liftweb.mapper._
import net.liftweb.util.Helpers._
import scala.collection.immutable.TreeMap
import scala.xml._
import S._
import net.liftweb.util._
import JE._

import com.liftcode._
import java.lang.{Number => ANumber}
import model._

class Sheet(info: CometActorInitInfo) extends CometActor(info) {
  def defaultPrefix = "ss"
  private var sheet: Map[(Int, Int), Any] = Map.empty
  
  private def cellFor(row: Int, col: Int): Node = 
  sheet.get((row, col)).map(_.toString).getOrElse("") match {
    case "" => <xml:group>&nbsp;</xml:group>
    case s => Text(s)
  }
  
  override def handleJson(in: Any): JsCmd = {
    in match {
    case JsonCmd("setcell", _ , (row: ANumber) :: (col: ANumber) :: (value: String) :: _, _) =>
    Model ! SetCell(row.intValue, col.intValue, value)
    
    case _ =>
  }
  Noop
  }
  
  private def id(row: Int, col: Int) = "r_"+row+"_c_"+col
  
  def render = <xml:group>
  {
    Script(javaScriptStuff)
  }
  <div style="position: relative">
  <table style="width: 2640px">
  <colgroup>
  <col width="40px"/>
  {
  (0 to 25).map(i => <col width="100px"/>)
  }
  </colgroup>
  <thead>
  <tr height="25px"><td>&nbsp;</td>
  {
    (0 to 25).map(i => <th width="100px">{('A' + i).toChar}</th>)
  }
  </tr>
  </thead>
  {
    (0 to 99).map(row => <tr heigth="25px"><td>{row + 1}</td>
    {
      (0 to 25).map(col => <td onclick={"editCell("+row+", "+col+")"} id={id(row, col)}>{cellFor(row,col)}</td>)
    }
    </tr>
    )
  }
  </table>
  <input id="thebox" onkeypress="keypressIt(event)" style="display: none; position: absolute; width: 100px; height: 25px; left: 240px; top: 50px;"/>
  </div>
  </xml:group>
  
  override def localSetup {
    Model !? AddListener(this) match {
      case m: Map[(Int, Int), Any] => sheet = m
    }
    
    super.localSetup
  }
  
  override def localShutdown {
    Model ! RemoveListener(this)
    super.localShutdown
  }
  
  override def lowPriority = {
    case UpdateSheet(newSheet, deltas) => 
    sheet = newSheet
    
    partialUpdate(deltas.map{case ((row, col), _) => 
    SetHtml(id(row, col), cellFor(row, col))}.foldLeft(Noop)(_ & _))
  } 

private def javaScriptStuff = 	JsRaw("""
  var theRow = 0;
  var theCol = 0;
  
  function editCell(row, col) {
   var inp = document.getElementById("thebox");
   inp.style.display = 'inline';
   theRow = row;
   theCol = col;
   inp.style.top = ""+(25 + 27 * theRow)+"px";
   inp.style.left = ""+(40 + 100 * theCol)+"px";
   inp.value = "";
   inp.select();
  }
  
  function keypressIt(e) {
    var characterCode = 0; // literal character code will be stored in this variable
    
    if(e && e.which){ //if which property of event object is supported (NN4)
      characterCode = e.which; //character code is contained in NN4's which property
    }
    else{
      e = event;
      characterCode = e.keyCode; //character code is contained in IE's keyCode property
    }
    
    if(characterCode == 13){ //if generated character code is equal to ascii 13 (if enter key)
      var inp = document.getElementById("thebox");
      """+
      jsonCall("setcell", 
		  JsArray(JsRaw("theRow"), JsRaw("theCol"), JsRaw("inp.value"))).toJsCmd
      +"""
      inp.style.display = 'none';
      return false;
    }
    else{
      return true;
    }
    
  }
  """)
}
