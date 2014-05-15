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

package fr.geocite.marius.state

import fr.geocite.marius.{ Marius, MariusLogging, MariusFile }
import scalaz.Lens
import scala.util.Random
import fr.geocite.marius.matching.PotentialMatrix
import fr.geocite.simpuzzle._
import scala.collection.mutable.ListBuffer
import fr.geocite.marius.structure._

object NetworkState {
  case class State(step: Int, cities: Seq[MariusCity.City], network: Network)
}

trait NetworkState <: MariusFile
    with MariusLogging
    with Marius
    with MariusCity
    with PotentialMatrix {

  def networkShare: Double

  type STATE = NetworkState.State

  def step = Lens.lensu[STATE, Int]((s, v) => s.copy(step = v), _.step)
  def cities = Lens.lensu[STATE, Seq[CITY]]((s, v) => s.copy(cities = v.toVector), _.cities)
  def network = Lens.lensu[STATE, Network]((s, v) => s.copy(network = v), _.network)

  def initialState(implicit rng: Random) = {
    val cities = initialCities
    NetworkState.State(0, cities, network(cities))
  }

  def network(cities: Seq[CITY]) = {
    def matrix =
      interactionPotentialMatrix(nbCities, supplies(cities), demands(cities), distanceMatrix, Network.full((0 until cities.size)))

    def indexedMatrix =
      for {
        (l, i) <- matrix.lines.zipWithIndex
        Matrix.Cell(j, v) <- l
      } yield (i, j, v)

    def nbKeep = math.ceil(networkShare * nbCities * (nbCities - 1)).toInt

    def kept = indexedMatrix.sortBy { case (_, _, ip) => ip }(Ordering[Double].reverse).take(nbKeep)

    val network = Seq.fill(nbCities)(ListBuffer.empty[Int])

    for {
      (i, j, _) <- kept
    } network(i) += j

    Network(network.map(_.toSeq))
  }
}

