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

import fr.geocite.marius.state.Network

object PotentialMatrix {
  case class InteractionPotentialException(message: String, matrix: Seq[Seq[Double]]) extends AssertionError(message)
}

trait PotentialMatrix <: InteractionPotential {

  def interactionPotentialMatrix(nbCities: Int, m1: Seq[Double], m2: Seq[Double], distances: Seq[Seq[Double]], network: Network): Array[Array[Double]] = {
    val iM1 = m1.toArray
    val iM2 = m2.toArray

    val potentials = Array.fill[Double](nbCities, nbCities)(0.0)

    for {
      i <- 0 until nbCities
      j <- network.outNode(i)
    } potentials(i)(j) = interactionPotential(iM1(i), iM2(j), distances(i)(j))
    potentials
  }

}
