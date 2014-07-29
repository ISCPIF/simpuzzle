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

package fr.geocites.marius.transaction

import fr.geocites.marius.Marius

import scala.util.Random

trait FixedCostTransaction <: Marius with ProportionalTransaction {

  /** Cost of a transaction */
  def fixedCost: Double

  /** Cost implied by the number of transactions */
  override def totalFixedCosts(t: Transacted): Seq[Double] =
    t.transacted.linesContent.map { _.count(_ > 0.0) * fixedCost }

  /** Filter the interaction potential matrix */
  override def transactions(state: STATE, supplies: Seq[Double], demands: Seq[Double])(implicit rng: Random) = {
    val interactionMatrixValue = super.transactions(state, supplies, demands)
    val fromInteractionPotentialSum = interactionMatrixValue.transpose.linesContent.map(_.sum)

    interactionMatrixValue.map {
      (from, to, ip) =>
        if (ip > 0) {
          val fSupply = supplies(from)
          val fromIPSum = fromInteractionPotentialSum(from)
          val normalisedIPFrom = ip / fromIPSum

          if (normalisedIPFrom * fSupply > fixedCost) ip else 0.0
        } else 0.0
    }
  }

}
