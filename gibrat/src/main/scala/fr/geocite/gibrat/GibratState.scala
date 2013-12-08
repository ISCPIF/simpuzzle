/*
 * Copyright (C) 08/12/13 Romain Reuillon
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

package fr.geocite.gibrat

import scala.util.Random
import scalaz._
import fr.geocite.simpuzzle.distribution.PopulationDistribution

trait GibratState <: Gibrat with PopulationDistribution {
  case class City(population: Double)

  case class GibratState(step: Int, populations: Seq[Double])

  type STATE = GibratState
  type CITY = City

  def initial(implicit rng: Random) = GibratState(0, populations(rng).take(nbCities).toSeq)

  def step = Lens.lensu[STATE, Int]((s, v) => s.copy(step = v), _.step)
  def population = Lens.lensu[CITY, Double]((c, v) => c.copy(population = v), _.population)

  def nbCities: Int
}
