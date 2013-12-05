/*
 * Copyright (C) 05/12/13 Romain Reuillon
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

import fr.geocite.marius.{ MariusFile, MariusState }
import fr.geocite.gibrat.GaussianGrowth
import scala.util.Random
import fr.geocite.simpuzzle.{TimeEndingCondition, StepByStep}

trait Marius <: StepByStep
    with TimeEndingCondition
    with MariusState
    with GaussianGrowth
    with MariusFile {
  def copy(c: CITY)(population: Double): CITY

  def step(s: STATE)(implicit rng: Random) = copy(s)(s.step + 1, cityGrowth(s))

  def cityGrowth(s: STATE)(implicit rng: Random) =
    s.cities.map { city => copy(city)(population = city.population * growthRate) }

  def nbCities: Int

}
