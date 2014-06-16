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

package fr.geocite.marius.matching

import fr.geocite.marius._
import fr.geocite.marius.structure.{ Network, Matrix }
import fr.geocite.marius.balance.FixedCost

trait FixedCostMatching <: ProportionalMatching with FixedCost { this: Marius =>

  override def interactionMatrix(supplies: Seq[Double], demands: Seq[Double], network: Network): Matrix = {
    val interactionMatrixValue = super.interactionMatrix(supplies, demands, network)
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
