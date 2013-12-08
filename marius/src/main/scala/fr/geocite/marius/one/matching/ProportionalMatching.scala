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

package fr.geocite.marius.one.matching

import scala.util.Random
import fr.geocite.simpuzzle.distribution._
import fr.geocite.marius.one._

trait ProportionalMatching <: Matching
    with InteractionPotential
    with Marius {

  def distanceOrderSell: Double

  def matchCities(
    s: STATE,
    supplies: Seq[Double],
    demands: Seq[Double])(implicit rng: Random) = {
    lazy val interactionMatrix = interactionPotentialMatrix(cities.get(s), supplies, distanceMatrix.get(s), distanceOrderSell)
    lazy val transactions = (interactionMatrix zip supplies).zipWithIndex.flatMap {
      case ((interactions, supply), from) =>
        interactions.normalise.map(_ * supply).zipWithIndex.map {
          case (q, to) => Transaction(from, to, q)
        }
    }

    val effectiveTransactedTo =
      transactions.groupBy(_.to).map {
        case (to, ts) =>
          val transactionsSum = ts.map(_.transacted).sum
          val coef = math.min(1.0, wealth.get(cities.get(s)(to)) / transactionsSum)
          to -> ts.map(t => t.copy(transacted = t.transacted * coef))
      }.withDefaultValue(Seq.empty)

    def unsatisfieds =
      for {
        (d, i) <- demands.zipWithIndex
        transactions = effectiveTransactedTo(i)
      } yield d - transactions.map(_.transacted).sum

    val effectiveTransactedFrom: Map[Int, Seq[Transaction]] =
      effectiveTransactedTo.toSeq.flatMap(_._2).groupBy(_.from).withDefaultValue(Seq.empty)

    def unsolds =
      for {
        (s, i) <- supplies.zipWithIndex
        transactions = effectiveTransactedFrom(i)
      } yield s - transactions.map(_.transacted).sum

    Matched(transactions, unsolds.toSeq, unsatisfieds.toSeq)
  }

}
