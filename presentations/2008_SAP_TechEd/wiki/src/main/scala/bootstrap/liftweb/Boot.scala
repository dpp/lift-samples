package bootstrap.liftweb

import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._
import Helpers._
import net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, ConnectionIdentifier}
import java.sql.{Connection, DriverManager}
import sample.model._

/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot {
  def boot {
    if (!DB.jndiJdbcConnAvailable_?) DB.defineConnectionManager(DefaultConnectionIdentifier, DBVendor)
    // where to search snippet
    LiftRules.addToPackages("sample")
    Schemifier.schemify(true, Log.infoF _, User, Wiki)

    LiftRules.rewrite.prepend {
      case RewriteRequest(ParsePath("view" :: what :: Nil, _, _, _), _, _) =>
        RewriteResponse(List("view"), Map("name" -> what))
        
      case RewriteRequest(ParsePath("edit" :: what :: Nil, _, _, _), _, _) =>
        RewriteResponse(List("edit"), Map("name" -> what))
        
       case RewriteRequest(ParsePath("index" :: Nil, _, _, _), _, _) =>
        RewriteResponse(List("view"), Map("name" -> "MainPage"))
    }
    
    // Build SiteMap
    val entries = Menu(Loc("View", List("view"), "View", Hidden)) :: 
    Menu(Loc("Edit", List("edit"), "Edit", Hidden, If(User.loggedIn_? _, 
                                                      "Must be logged in"))) ::
    User.sitemap

    LiftRules.setSiteMap(SiteMap(entries:_*))

    // LiftRules.addTemplateBefore(User.templates)


  }
}


object DBVendor extends ConnectionManager {
  def newConnection(name: ConnectionIdentifier): Box[Connection] = {
    try {
      Class.forName("org.apache.derby.jdbc.EmbeddedDriver")
      val dm = DriverManager.getConnection("jdbc:derby:lift_example;create=true")
      Full(dm)
    } catch {
      case e : Exception => e.printStackTrace; Empty
    }
  }
  def releaseConnection(conn: Connection) {conn.close}
}

