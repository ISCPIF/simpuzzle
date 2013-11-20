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

import fr.geocite.marius.one
import scala.util.Random

trait Zero <: one.MariusStep with one.MariusInitialState {

  case class ZeroCity(population: Double, wealth: Double, region: String, capital: Boolean, saving: Double) extends one.City
  type CITY = ZeroCity

  def copy(c: CITY)(population: Double = c.population, wealth: Double = c.wealth, saving: Double = c.saving): CITY =
    c.copy(population = population, wealth = wealth, saving = saving)

  def nbCities: Int

  def initial(implicit rng: Random) = MariusState(0, cities.take(nbCities).toSeq, distances)

  def cities(implicit rng: Random) =
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
