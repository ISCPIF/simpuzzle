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

package fr.geocites.marius.balance

import fr.geocites.marius.transaction.Transaction
import scala.util.Random
import fr.geocites.simpuzzle._
import fr.geocites.marius._
import fr.geocites.marius.structure.Matrix._
import fr.geocites.marius.structure.Matrix

trait Exchange <: Transaction { this: Marius =>

  case class Transacted(val s: STATE, val supplies: Seq[Double], val demands: Seq[Double], val transacted: Matrix) {
    lazy val transposedTransacted = transacted.transpose
    lazy val transactedFromSum = transacted.linesContent.map(_.sum)
    lazy val transactedToSum = transposedTransacted.linesContent.map(_.sum)
    lazy val nbCities = cities.get(s).size
  }

  def exchangeBalances(
    state: STATE,
    supplies: Seq[Double],
    demands: Seq[Double])(implicit rng: Random) = {

    val t = Transacted(state, supplies, demands, transactions(state, supplies, demands))

    def interactions =
      for {
        (l, i) <- t.transacted.lines.zipWithIndex
        Cell(j, v) <- l
      } yield Interaction(i, j, v)

    log(transactedBalances(t), interactions)
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

    (unsolds zip unsatisfieds zip bonuses(transacted) zip totalFixedCosts(transacted)).map(flatten).map {
      case (unsold, unsatisfied, bonus, totalFixedCost) => unsatisfied - unsold + bonus - totalFixedCost
    }
  }

  def bonuses(transacted: Transacted): Seq[Double] = transacted.supplies.map(_ => 0.0)
  def totalFixedCosts(transacted: Transacted): Seq[Double] = transacted.supplies.map(_ => 0.0)

}
