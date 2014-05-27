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

package fr.geocite.marius.balance

import fr.geocite.marius.matching.Matching
import scala.util.Random
import fr.geocite.simpuzzle._
import fr.geocite.marius._
import fr.geocite.marius.structure.Matrix._
import fr.geocite.marius.structure.Matrix

trait ExchangeBalances <: Matching { this: Marius =>

  case class Transacted(val s: STATE, val supplies: Seq[Double], val demands: Seq[Double], val transacted: Matrix) {
    lazy val transposedTransacted = transacted.transpose
    lazy val transactedFromSum = transacted.linesContent.map(_.sum)
    lazy val transactedToSum = transposedTransacted.linesContent.map(_.sum)
    lazy val nbCities = cities.get(s).size
  }

  def exchangeBalances(
    s: STATE,
    supplies: Seq[Double],
    demands: Seq[Double])(implicit rng: Random) = {

    val t = Transacted(s, supplies, demands, matchCities(s, supplies, demands))

    def transactions =
      for {
        (l, i) <- t.transacted.lines.zipWithIndex
        Cell(j, v) <- l
      } yield Transaction(i, j, v)

    log(transactedBalances(t), transactions)
  }

  def transactedBalances(transacted: Transacted) = {
    def unsatisfieds =
      for {
        (d, i) <- transacted.demands.zipWithIndex
      } yield {
        val unsatisfied = d - transacted.transactedToSum(i)
        if (unsatisfied >= 0) unsatisfied else 0
      }

    def unsolds =
      for {
        (s, i) <- transacted.supplies.zipWithIndex
      } yield {
        val unsold = s - transacted.transactedFromSum(i)
        if (unsold >= 0) unsold else 0
      }

    (unsolds zip unsatisfieds).map(flatten).map {
      case (unsold, unsatisfied) => unsatisfied - unsold
    }
  }

}
