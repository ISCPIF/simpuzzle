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

trait Basic <: Marius with PopulationDistribution {
  case class City(population: Double) extends Population
  case class GibratState(step: Int, cities: Seq[City])

  type CITY = City
  type STATE = GibratState

  def initial(implicit rng: Random) = GibratState(0, populations.take(nbCities).map(City(_)).toSeq)
  def copy(c: CITY)(p: Double) = c.copy(p)
  def copy(s: STATE)(step: Int, cities: Seq[City]) = s.copy(step, cities)
}