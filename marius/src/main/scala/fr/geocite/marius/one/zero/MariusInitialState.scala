/*
 * Copyright (C) 27/06/13 Romain Reuillon
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

package fr.geocite.marius.one.zero

import fr.geocite.simpuzzle.InitialState
import fr.geocite.simpuzzle.distribution.{ PositionDistribution, PopulationDistribution }
import fr.geocite.marius.{ CapitalDistribution, RegionDistribution }
import scala.util.Random
import fr.geocite.gis.distance.GeodeticDistance

trait MariusInitialState <: InitialState
    with MariusState
    with PopulationDistribution
    with RegionDistribution
    with CapitalDistribution
    with PositionDistribution
    with InitialWealth
    with GeodeticDistance {

  def initial(implicit rng: Random) = MariusState(0, cities, distances)

  def cities(implicit rng: Random) =
    for {
      ((p, r), c) <- populations zip regions zip capitals
    } yield {
      City(
        population = p,
        region = r,
        capital = c,
        wealth = initialWealth(p)
      )
    }

  def distances(implicit rng: Random) = {
    val positions = positionDistribution(rng).toIndexedSeq

    positions.zipWithIndex.map {
      case (c1, i) =>
        positions.zipWithIndex.map { case (c2, _) => distance(c1, c2) }
    }
  }

}
