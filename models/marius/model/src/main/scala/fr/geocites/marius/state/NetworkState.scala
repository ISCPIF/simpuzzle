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

package fr.geocites.marius.state

import fr.geocites.marius._
import scala.util.Random
import fr.geocites.marius.transaction.PotentialMatrix
import scala.collection.mutable.ListBuffer
import fr.geocites.marius.structure._
import monocle.Macro._
import monocle._

object NetworkState {
  case class State(step: Int, cities: Seq[MariusCity.City], network: Network, distanceMatrix: DistanceMatrix)
}

trait NetworkState <: MariusLogging
    with Marius
    with MariusCity
    with PotentialMatrix {

  def networkShare: Double

  type STATE = NetworkState.State

  def step = mkLens[STATE, Int]("step")

  def cities = SimpleLens[STATE, Seq[CITY]](_.cities, (s, v) => s.copy(cities = v.toVector))
  def network = mkLens[STATE, Network]("network")
  def distances = mkLens[STATE, DistanceMatrix]("distanceMatrix")

  def initialState(implicit rng: Random) = {
    val cities = initialCities
    NetworkState.State(0, cities, Network.full(cities.size), MariusFile.distanceMatrix)
  }

  def network(cities: Seq[CITY]) = {
    val fullNetworkInitialState = NetworkState.State(0, cities, Network.full(cities.size), MariusFile.distanceMatrix)

    def matrix =
      interactionPotentialMatrix(fullNetworkInitialState, supplies(cities), demands(cities))

    def indexedMatrix =
      for {
        (l, i) <- matrix.lines.zipWithIndex
        Matrix.Cell(j, v) <- l
      } yield (i, j, v)

    def nbKeep = math.ceil(networkShare * cities.size * (cities.size - 1)).toInt

    def kept = indexedMatrix.sortBy { case (_, _, ip) => ip }(Ordering[Double].reverse).take(nbKeep)

    val network = Seq.fill(cities.size)(ListBuffer.empty[Int])

    for {
      (i, j, _) <- kept
    } network(i) += j

    Network(network.map(_.toSeq))
  }
}

