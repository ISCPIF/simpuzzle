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

import fr.geocite.marius._
import fr.geocite.simpuzzle._

object InteractionPotential {
  case class InteractionPotentialException(message: String, matrix: Seq[Seq[Double]]) extends AssertionError(message)
}

trait InteractionPotential <: Marius {

  def distanceDecay: Double

  def interactionPotentialMatrix(cities: Seq[CITY], masses: Seq[Double], distances: Seq[Seq[Double]]) = {
    val citiesWithSupply = cities zip masses
    val interactionMatrix = citiesWithSupply.zipWithIndex.toIndexedSeq.map {
      case ((c1, s1), i) =>
        citiesWithSupply.zipWithIndex.toIndexedSeq.map {
          case ((c2, s2), j) =>
            if (i == j) 0.0
            else interactionPotential(s1, s2, distances(i)(j))
        }

    }
    check(
      interactionMatrix.flatten.count(ip => ip > 0) >= 2,
      s"Interaction potential matrix is empty",
      InteractionPotential.InteractionPotentialException(_, interactionMatrix)
    )
    interactionMatrix
  }

  def interactionPotential(supply1: Double, supply2: Double, distance: Double) = {
    val potential = (supply1 * supply2) / math.pow(distance, distanceDecay)
    check(potential >= 0, s"Error in potential computing gave $potential for $supply1 $supply2 $distance")
    potential
  }

}