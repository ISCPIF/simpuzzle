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
import fr.geocite.simpuzzle.matrix.TriangularMatrix
import fr.geocite.marius.structure.Network

trait SymmetricPotentialMatrix <: InteractionPotential {

  def interactionPotentialMatrix(nbCities: Int, masses: Seq[Double], distances: Seq[Seq[Double]], network: Network): TriangularMatrix[Double] = {
    val indexedMasses = masses.toArray
    val interactionLines = Array.ofDim[Array[Double]](nbCities)

    var i = 0

    while (i < nbCities) {
      var j = i + 1
      val line = Array.ofDim[Double](nbCities - 1 - i)
      while (j < nbCities) {
        line(j - i - 1) =
          if (network.existsOut(i, j)) interactionPotential(indexedMasses(i), indexedMasses(j), distances(i)(j))
          else 0.0
        j += 1
      }
      interactionLines(i) = line
      i += 1
    }

    TriangularMatrix(interactionLines, 0.0)
  }

}