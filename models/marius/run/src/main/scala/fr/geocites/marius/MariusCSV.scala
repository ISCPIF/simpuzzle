/*
 * Copyright (C) 15/01/14 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.geocites.marius

import fr.geocites.gugus.Gugus
import fr.geocites.gugus.tool.ToCSV
import fr.geocites.simpuzzle._
import scalax.io.Resource
import java.io.File
import util.{ Success, Failure }
import monocle.syntax._

object MariusCSV extends App with ToCSV {
  val path = new File("/tmp/mariusmodel_log.csv")
  println(Console.YELLOW + "Choose your model: ")
  Models.all.map(_.getClass.getName).zipWithIndex.foreach { case (c, i) => println(Console.GREEN + s"$i -> ${Console.GREEN} $c") }
  val i = io.StdIn.readInt()
  print(Console.RESET)
  toCSVFile(Models.all(i), path)
}
