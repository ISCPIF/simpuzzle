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

import fr.geocites.simpuzzle._
import scalax.io.Resource
import java.io.File
import util.{ Success, Failure }
import monocle.syntax._

object MariusCSV extends App {

  println(Console.YELLOW + "Choose you model: ")
  Models.all.map(_.getClass.getName).zipWithIndex.foreach { case (c, i) => println(Console.GREEN + s"$i -> ${Console.GREEN} $c") }
  val i = io.StdIn.readInt()
  print(Console.RESET)

  run(Models.all(i))

  def run(m: Marius) = {
    implicit val rng = fr.geocites.simpuzzle.random(42)
    import m._

    val path = "/tmp/mariusmodel_log.csv"

    new File(path).delete

    val out = Resource.fromFile(path)

    out.append("step, arokato, population, wealth, supply, demand, transactedFrom, transactedTo \n")

    for {
      (log, step) <- m.logs zipWithIndex
    } log.value match {
      case Success(s) =>
        val cs = s |-> cities get
        val transacted = log.written
        val from = transacted.groupBy(_.from)
        val to = transacted.groupBy(_.to)

        for {
          (city, arokato, s, d, i) <- (
            cs zip
            m.arokatos zip
            m.supplies(cs) zip
            m.demands(cs)).zipWithIndex.map(flatten)
        } {
          def line =
            Seq(
              step,
              arokato,
              city |-> population get,
              city |-> wealth get,
              s,
              d,
              from.getOrElse(i, Seq.empty).map(_.transacted).sum,
              to.getOrElse(i, Seq.empty).map(_.transacted).sum
            )
          out.append(line.mkString(",") + "\n")
        }
        val totalWealth = cs.map(_ |-> wealth get).sum
        val totalPop = cs.map(_ |-> population get).sum

        println(s"step $step, total wealth $totalWealth, total population $totalPop")

      case Failure(e) => println(s"Invalid State $e")
    }
  }

}
