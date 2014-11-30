/*
 * Copyright (C) 2014 Romain Reuillon
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

package fr.geocites.gugus.tool

import java.io.File

import fr.geocites.gugus.Gugus

import scala.util.{ Failure, Success }
import scalax.io.Resource
import fr.geocites.simpuzzle._
import monocle.syntax._

trait ToCSV {

  def toCSVFile(m: Gugus { def arokatos: Seq[String] }, file: File) = {
    file.delete
    val out = Resource.fromFile(file)
    out.append("step, arokato, population, wealth, supply, demand, transactedFrom, transactedTo \n")
    toCSV(m).foreach { l => out.append(l + "\n") }
  }

  def toCSV(m: Gugus { def arokatos: Seq[String] }) = {
    implicit val rng = fr.geocites.simpuzzle.random(42)
    import m._

    def csv: Iterator[Iterator[String]] =
      for {
        (log, step) <- m.logs zipWithIndex
      } yield log.value match {
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
              m.demands(cs)).zipWithIndex.map(flatten).toIterator
          } yield {
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
            line.mkString(",")
          }
        case Failure(e) => Iterator.empty
      }

    csv.flatten
  }
}
