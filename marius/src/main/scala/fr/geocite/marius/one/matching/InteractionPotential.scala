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

import fr.geocite.marius.one.City

trait InteractionPotential {

  def distanceDecay: Double

  def interactionPotentialMatrix(s: Seq[City], masses: Seq[Double], distances: Seq[Seq[Double]], beta: Double) = {
    val citiesWithSupply = s zip masses
    citiesWithSupply.zipWithIndex.toIndexedSeq.map {
      case ((c1, s1), i) =>
        citiesWithSupply.zipWithIndex.toIndexedSeq.flatMap {
          case ((c2, s2), j) =>
            if (i == j) None
            else Some(interactionPotential(s1, s2, distances(i)(j), beta))
        }
    }
  }

  def interactionPotential(supply1: Double, supply2: Double, distance: Double, beta: Double) = {
    val potential = math.pow(supply1 * supply2, 1 - beta) / math.pow(math.pow(distance, distanceDecay), beta)
    assert(potential >= 0, s"Error in potential computing gave $potential for $supply1 $supply2 $distance $beta")
    potential
  }
}
