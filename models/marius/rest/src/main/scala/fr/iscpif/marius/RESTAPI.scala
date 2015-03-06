/*
 * Copyright (C) 2015 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.iscpif.marius

import javax.script.ScriptEngineManager
import javax.servlet.ServletContext

import org.eclipse.jetty.server.Server
import org.eclipse.jetty.servlet.DefaultServlet
import org.eclipse.jetty.webapp.WebAppContext
import org.scalatra._
import org.scalatra.servlet.ScalatraListener

import scala.util.Random

object RESTAPI extends App {
  val p = if (args.size >= 1) args(0).toInt else 8080
  val server = new Server(p)
  val context = new WebAppContext()
  context setContextPath "/"
  context.setResourceBase("src/main/webapp")
  context.addEventListener(new ScalatraListener)
  context.setInitParameter(ScalatraListener.LifeCycleKey, classOf[ScalatraBootstrap].getName)
  context.addServlet(classOf[DefaultServlet], "/")
  server.setHandler(context)
  server.start()
}

class ScalatraExample extends ScalatraServlet {

  get("/run/:family") {
    implicit val rng = new Random(42)
    val family = Families.all(params("family"))
    val mechanisms = params("mechanisms").split(",").toSet
    val id = family.traitsCombinations.map(_.map(_.getCanonicalName)).zipWithIndex.find {
      case (ts, _) =>
        ts.toSet == mechanisms
    }.getOrElse(throw new RuntimeException(s"Model with mechanisms $mechanisms doesn't exist"))._2
    val parameters =
      params("parameters").split(",").map {
        p =>
          val Array(name, value) = p.split("=")
          name -> value.toDouble
      }.toMap
    val parameterValues = family.attributes.map(parameters(_))
    family.run(id, parameterValues: _*).get("csv")
  }
}

class ScalatraBootstrap extends LifeCycle {
  override def init(context: ServletContext) {
    context mount (new ScalatraExample, "/*")
  }
}
