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
            val transacted = (ip / interactionPotentialSum) * supplies(from)
            Transaction(from, to, transacted)
        }
    }

    val transposedTransactions = transactions.transpose

    def unsatisfieds =
      for {
        (d, i) <- demands.zipWithIndex
        transactionsTo = transposedTransactions(i)
      } yield d - transactionsTo.map(_.transacted).sum match {
        case x if x <= 0 => 0
        case x => x
      }

    def unsolds =
      for {
        (s, i) <- supplies.zipWithIndex
        transactionsFrom = transactions(i)
      } yield {
        val unsold = s - transactionsFrom.map(_.transacted).sum
        assert(unsold > 0)
        unsold
      }

    Matched(transactions.flatten, cities.get(s).map(c => 0.0), unsatisfieds.toSeq)
  }

}
