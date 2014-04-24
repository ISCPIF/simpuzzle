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

    lazy val toInteractionPotentialSums = interactionMatrix.transpose.map(_.sum)
    lazy val fromInteractionPotentialSums = interactionMatrix.map(_.sum)

    interactionMatrix.zipWithIndex.map {
      case (interactions, from) =>
        val fromIPSum = fromInteractionPotentialSums(from)
        interactions.zipWithIndex.map {
          case (ip, to) =>
            if (ip <= 0.0) Transaction(from, to, 0.0)
            else {
              val fSupply = supplies(from)
              val tDemand = demands(to)
              val tSupply = supplies(to)
              val toIPSum = toInteractionPotentialSums(to)

              check(fSupply >= 0 && tDemand >= 0, s"supply or demand not good, $fSupply $tDemand")

              val normalisedIPFrom = ip / fromIPSum
              val normalisedIPTo = ip / toIPSum

              val transacted = min(normalisedIPFrom * fSupply, normalisedIPTo * tDemand)
              check(
                !transacted.isNaN, s"Transacted is NaN: from $from to $to , ip%from : $normalisedIPFrom supplyfrom  $fSupply todemand $tDemand ip%to $normalisedIPTo  fromipsum $fromIPSum toipsum $toIPSum suppllies du to $tSupply",
                InteractionPotential.InteractionPotentialException(_, interactionMatrix)
              )
              Transaction(from, to, transacted)
            }
        }
    }
  }

}
