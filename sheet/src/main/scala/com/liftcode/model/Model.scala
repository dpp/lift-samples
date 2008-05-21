package com.liftcode.model

import scala.actors._
import Actor._
import net.liftweb._
import util._
import Helpers._
import java.util.concurrent._

object Model extends Actor with Model with Evaluator with Arithmetic {
  private var current: Map[(Int, Int), Any] = Map.empty
  private var listeners: List[Actor] = Nil
  
  def act = loop {
    createCurrent()
    react {
      case AddListener(who) =>
      listeners = who :: listeners
      reply(current)
      
      case RemoveListener(who) =>
      listeners = listeners.filter(_ ne who)
      
      case SetCell(row, col, value) => cells(row)(col).formula = FormulaParsers.parse(value)
      val delta = createCurrent()
      listeners.foreach(_ ! UpdateSheet(current, delta))
      
      case _ =>
    }
  }
  
  private def createCurrent(): List[((Int, Int), Any)] = {
    var tm: Map[(Int, Int), Any] = Map.empty
    for (row <- 0 until height; col <- 0 until width) tm += ((row, col) -> cells(row)(col).toString)
    val delta = tm.elements.toList -- current.elements.toList
    current = tm
    delta
  }
  
  this.start
  
  def width = 26
  def height = 100
  
}

case class AddListener(who: Actor)
case class RemoveListener(who: Actor)
case class UpdateSheet(newSheet: Map[(Int, Int), Any], deltas: List[((Int, Int), Any)])
case class SetCell(row: Int, col: Int, value: String)

import scala.util.parsing.combinator._

trait Formula
case class Coord(row: Int, column: Int) extends Formula {
  override def toString = ('A' + column).toChar.toString+row
}
case class Range(c1: Coord, c2: Coord) extends Formula {
  override def toString = c1.toString+":"+c2.toString
}
case class Number(value: Double) extends Formula {
  override def toString = value.toString
}
case class Textual(value: String) extends Formula {
  // override def toString = value.toString
}
case class Application(function: String,
arguments: List[Formula]) extends Formula {
  override def toString =
  function+arguments.mkString("(", ",", ")")
}
object Empty extends Textual("")

object FormulaParsers extends RegexParsers {
  def ident: Parser[String] = """[a-zA-Z_]\w*""".r
  def decimal: Parser[String] = """-?\d+(\.\d*)?""".r
  def cell: Parser[Coord] =
  """[A-Za-z]\d+""".r ^^ {
    s => 
    val column = s.toUpperCase.charAt(0) - 'A'
    val row = s.substring(1).toInt - 1
    
    Coord(row, column)
  }
  
  def range: Parser[Range] =
  cell~":"~cell ^^ {
    case c1~":"~c2 => Range(c1, c2)
  }
  def number: Parser[Number] =
  decimal ^^ {n => Number(n.toDouble)}
  def application: Parser[Application] =
  ident~"("~repsep(expr, ",")~")" ^^ {
    case f~"("~ps~")" => Application(f, ps)
  }
  def expr: Parser[Formula] =
  range | cell  | number | application
  def textual: Parser[Textual] =
  """[^=].*""".r ^^ Textual
  def formula: Parser[Formula] =
  number  | textual | "="~>expr
  def parse(input: String): Formula =
  parseAll(formula, input.trim) match {
    case Success(e, _) =>  e
    case f: NoSuccess => Textual("["+f.msg+"]")
  }
}

trait Evaluator {
  this: Model =>
  def evaluate(e: Formula): Double = 
  try {
    e match {
      case Coord(row, column) =>
      cells(row)(column).value
      case Number(v) =>
      v
      case Textual(_) =>
      0
      case Application(function, arguments) =>
	    val argvals = arguments flatMap evalList
      operations(function)(argvals)
    }
  } catch {
    case ex: Throwable => Math.NaN_DOUBLE
  }
  
  type Op = List[Double] => Double
  val operations = new collection.mutable.HashMap[String, Op]
  
  private def evalList(e: Formula): List[Double] = e match {
    case Range(_, _) => references(e) map (_.value)
    case _ => List(evaluate(e))
  }
  
  def references(e: Formula): List[Cell] = e match {
    case Coord(row, column) =>
    List(cells(row)(column))
    case Range(Coord(r1, c1), Coord(r2, c2)) =>
    for (row <- (r1 to r2).toList; column <- c1 to c2)
    yield cells(row)(column)
    case Application(function, arguments) =>
    arguments flatMap references
    case _ =>
    List()
  }
  
}

trait Model {
  this: Evaluator =>
  case class Cell(row: Int, column: Int) {
    var formula: Formula = Empty
    def value = evaluate(formula)
    
    override def toString = formula match {
      case Textual(s) => s
      case _ => value.toString
    }
  }
  val cells = new Array[Array[Cell]](height, width)
  for (i <- 0 until height; j <- 0 until width)
  cells(i)(j) = new Cell(i, j)
  
  def width: Int
  def height: Int
}


trait Arithmetic {
  this: Evaluator =>
  operations += (
  "add"  -> { case List(x, y) => x + y },
  "sub"  -> { case List(x, y) => x - y },
  "div"  -> { case List(x, y) => x / y },
  "mul"  -> { case List(x, y) => x * y },
  "mod"  -> { case List(x, y) => x % y },
  "sum"  -> { xs => (0.0 /: xs)(_ + _) },
  "prod" -> { xs => (1.0 /: xs)(_ * _) }
  )
}

