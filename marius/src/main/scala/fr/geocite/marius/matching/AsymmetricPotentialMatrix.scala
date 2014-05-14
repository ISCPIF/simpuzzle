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

trait AsymmetricPotentialMatrix <: InteractionPotential {

  def interactionPotentialMatrix(nbCities: Int, m1: Seq[Double], m2: Seq[Double], distances: Seq[Seq[Double]]): Array[Array[Double]] = {
    val iM1 = m1.toArray
    val iM2 = m2.toArray

    val potentials = Array.ofDim[Double](nbCities, nbCities)

    var i = 0
    while (i < nbCities) {
      var j = 0
      while (j < nbCities) {
        potentials(i)(j) =
          if (i != j) interactionPotential(iM1(i), iM2(j), distances(i)(j)) else 0.0
        j += 1
      }
      i += 1
    }

    potentials
  }

}
