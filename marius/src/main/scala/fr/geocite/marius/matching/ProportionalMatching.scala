/*
 * Copyright (C) 23/10/13 Romain Reuillon
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

package fr.geocite.marius.matching

import scala.util.Random
import fr.geocite.marius.{ Transaction, Marius }
import scala.math._
import fr.geocite.simpuzzle._

trait ProportionalMatching <: Matching
    with InteractionPotential
    with Marius {

  def matchCities(
    s: STATE,
    supplies: Seq[Double],
    demands: Seq[Double])(implicit rng: Random) = {

    lazy val interactionMatrix =
      interactionPotentialMatrix(
        cities.get(s),
        supplies,
        distanceMatrix.get(s))

    lazy val transactions = interactionMatrix.zipWithIndex.map {
      case (interactions, from) =>
        val interactionPotentialSum = interactions.sum
        interactions.zipWithIndex.map {
          case (ip, to) =>
            if (interactionPotentialSum <= 0.0) Transaction(from, to, 0.0)
            else {
              val fSupply = supplies(from)
              val dDemand = demands(to)
              check(fSupply >= 0 && dDemand >= 0, s"supply or demand not good, $fSupply $dDemand")
              val transacted =
                min((ip / interactionPotentialSum) * fSupply, (ip / interactionPotentialSum) * dDemand)
              check(!transacted.isNaN, s"Transacted is NaN $ip $interactionPotentialSum $fSupply $dDemand")
              Transaction(from, to, transacted)
            }
        }
    }

    val transposedTransactions = transactions.transpose

    def unsatisfieds =
      for {
        (d, i) <- demands.zipWithIndex
        transactionsTo = transposedTransactions(i)
      } yield {
        val transactedSum = transactionsTo.map(_.transacted).sum
        val unsatisfied = d - transactedSum
        check(unsatisfied >= 0, s"unsatisfied not good, $unsatisfied $d $transactedSum")
        unsatisfied
      }

    def unsolds =
      for {
        (s, i) <- supplies.zipWithIndex
        transactionsFrom = transactions(i)
      } yield {
        val unsold = s - transactionsFrom.map(_.transacted).sum
        check(unsold >= 0, s"unsold not good, $unsold")
        unsold
      }

    Matched(transactions.flatten, unsolds.toSeq, unsatisfieds.toSeq)
  }

}
