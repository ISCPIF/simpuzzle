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

package fr.geocite.marius.one

import scala.util.Random
import scalaz._

trait Basic <: Marius {

  case class ZeroCity(population: Double, wealth: Double, region: String, capital: Boolean, saving: Double)
  type CITY = ZeroCity

  def population = Lens.lensu[CITY, Double]((c, v) => c.copy(population = v), _.population)
  def wealth = Lens.lensu[CITY, Double]((c, v) => c.copy(wealth = v), _.wealth)
  def capital = Lens.lensu[CITY, Boolean]((c, v) => c.copy(capital = v), _.capital)
  def saving = Lens.lensu[CITY, Double]((c, v) => c.copy(saving = v), _.saving)
  def region = Lens.lensu[CITY, String]((c, v) => c.copy(region = v), _.region)

  case class MariusState(step: Int, cities: Seq[CITY], distanceMatrix: DistanceMatrix)
  type STATE = MariusState

  def step = Lens.lensu[STATE, Int]((s, v) => s.copy(step = v), _.step)
  def cities = Lens.lensu[STATE, Seq[CITY]]((s, v) => s.copy(cities = v), _.cities)
  def distanceMatrix = Lens.lensu[STATE, DistanceMatrix]((s, v) => s.copy(distanceMatrix = v), _.distanceMatrix)

  def nbCities: Int

  def initialState(implicit rng: Random) = MariusState(0, initialCities.take(nbCities).toSeq, distances)

  def initialCities(implicit rng: Random) =
    for {
      ((p, r), c) <- populations zip regions zip capitals
    } yield {
      ZeroCity(
        population = p,
        region = r,
        capital = c,
        wealth = initialWealth(p),
        saving = 0
      )
    }

}
