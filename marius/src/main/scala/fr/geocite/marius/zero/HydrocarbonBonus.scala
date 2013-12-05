/*
 * Copyright (C) 14/05/13 Romain Reuillon
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

package fr.geocite.marius.zero

import fr.geocite.simpuzzle.city.Population
import scala.util.Random
import fr.geocite.simpuzzle.distribution.PopulationDistribution
import fr.geocite.marius.{ HydrocarbonDistribution, Hydrocarbon }

trait HydrocarbonBonus <: Marius
    with PopulationDistribution
    with HydrocarbonDistribution {

  case class City(population: Double, hydrocarbon: Boolean) extends Population with Hydrocarbon
  case class MariusState(step: Int, cities: Seq[City])

  def copy(c: CITY)(p: Double) = c.copy(p)
  def copy(s: STATE)(step: Int, cities: Seq[City]) = s.copy(step, cities)

  type STATE = MariusState
  type CITY = City

  /// Annual mean growth rate
  def rate: Double
  def stdRate: Double
  def hydrocarbonBonus: Double

  def growthRate(c: Hydrocarbon)(implicit rng: Random) = {
    val bonus = if (c.hydrocarbon) hydrocarbonBonus else 0.0
    1 + (stdRate * rng.nextGaussian + rate + bonus)
  }

  def nbCities: Int

  def initial(implicit rng: Random) = {
    val cities = (populations zip hydrocarbons).map {
      case (p, h) => City(p, h)
    }
    MariusState(0, cities.take(nbCities).toSeq)
  }
}

