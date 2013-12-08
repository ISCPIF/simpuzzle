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

import scala.util.Random
import fr.geocite.simpuzzle.distribution.PopulationDistribution
import fr.geocite.marius.{ HydrocarbonDistribution, Hydrocarbon }
import scalaz.Lens

trait HydrocarbonBonus <: Marius
    with PopulationDistribution
    with HydrocarbonDistribution {

  case class City(population: Double, hydrocarbon: Boolean)
  case class State(step: Int, cities: Seq[City])

  type STATE = State
  type CITY = City

  def step = Lens.lensu[State, Int]((s, v) => s.copy(step = v), _.step)
  def cities = Lens.lensu[State, Seq[CITY]]((s, v) => s.copy(cities = v), _.cities)
  def hydrocarbon = Lens.lensu[CITY, Boolean]((c, v) => c.copy(hydrocarbon = v), _.hydrocarbon)
  def population = Lens.lensu[CITY, Double]((c, v) => c.copy(population = v), _.population)

  def hydrocarbonBonus: Double

  override def growthRate(c: CITY)(implicit rng: Random) = {
    val bonus = if (hydrocarbon.get(c)) hydrocarbonBonus else 0.0
    1 + (stdRate * rng.nextGaussian + rate + bonus)
  }

  def nbCities: Int

  def initialState(implicit rng: Random) = {
    val cities = (populations(rng) zip hydrocarbons(rng)).map {
      case (p, h) => City(p, h)
    }
    State(0, cities.take(nbCities).toSeq)
  }

}

