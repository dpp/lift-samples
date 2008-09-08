/*
 * Page.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package sample.snippet

import net.liftweb.http._
import SHtml._
import S._
import net.liftweb.mapper._
import net.liftweb.util._
import Helpers._
import sample.model._
import scala.xml._
import net.liftweb.textile._

class Page {
  def edit(xhtml: Group) = {
    val name = param("name").openOr("Home")
    val entry = Wiki.find(By(Wiki.name, name)).openOr(Wiki.create.name(name))

    bind("edit", xhtml,
        "name" -> name,
        "area" -> textarea(entry.entry, value => {entry.entry(value).save; redirectTo("/view/"+urlEncode(name))}) %
          ("rows" -> "10") % ("cols" -> 35))
  }

  def view(xhtml: Group) = {
    val name = param("name").openOr("Home")
    val content = Wiki.find(By(Wiki.name, name)).map(_.entry.is).
      openOr("__This entry does not exist, click \"edit\" to create it__")

    bind("view", xhtml,
        "name" -> name,
        "login" -> ((xml: NodeSeq) => if (!User.loggedIn_?) xml else Text("")),
        "edit" -> ((xml: NodeSeq) => if (User.loggedIn_?) bind("view", xml,
             FuncAttrBindParam("editref", node => Text(node.text+name), "href")) else Text("")),
        "content" -> TextileParser.toHtml(content, Some(DefaultRewriter("/view"))))
  }
}


