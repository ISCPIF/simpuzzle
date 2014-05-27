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
import fr.geocite.marius.structure.Matrix._

trait ProportionalMatching <: Matching
    with PotentialMatrix
    with Marius { this: Marius =>

  def matchCities(
    s: STATE,
    supplies: Seq[Double],
    demands: Seq[Double])(implicit rng: Random) = {

    val indexedSupplies = supplies.toArray
    val indexedDemands = demands.toArray

    val interactionMatrix =
      interactionPotentialMatrix(
        nbCities,
        supplies,
        demands,
        distanceMatrix,
        network.get(s))

    val fromInteractionPotentialSum = interactionMatrix.linesContent.map(_.sum)
    val toInteractionPotentialSum = interactionMatrix.transpose.linesContent.map(_.sum)

    interactionMatrix.map {
      (from, to, ip) =>
        if (ip > 0) {
          val fSupply = indexedSupplies(from)
          val tDemand = indexedDemands(to)
          val toIPSum = toInteractionPotentialSum(to)
          val fromIPSum = fromInteractionPotentialSum(from)
          check(fSupply >= 0 && tDemand >= 0, s"supply or demand not good, $fSupply $tDemand")

          val normalisedIPFrom = ip / fromIPSum
          val normalisedIPTo = ip / toIPSum

          val t = min(normalisedIPFrom * fSupply, normalisedIPTo * tDemand)
          check(
            !t.isNaN, s"Transacted is NaN: from $from to $to , ip%from : $normalisedIPFrom supplyfrom  $fSupply todemand $tDemand ip%to $normalisedIPTo  fromipsum $fromIPSum toipsum $toIPSum",
            PotentialMatrix.InteractionPotentialException(_, interactionMatrix)
          )
          t
        } else 0.0
    }
  }

}
