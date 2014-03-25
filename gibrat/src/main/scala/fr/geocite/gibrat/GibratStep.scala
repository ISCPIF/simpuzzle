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

package fr.geocite.gibrat

import scala.util.Random
import fr.geocite.simpuzzle.state
import scalaz._
import fr.geocite.simpuzzle.state.Step
import fr.geocite.simpuzzle.logging.NoLogging

trait GibratStep <: Step
    with NoLogging
    with state.State {

  type CITY

  def nextState(s: STATE)(implicit rng: Random) =
    cities.mod(_.map(c => population.mod(_ * growthRate(c), c)), s)

  def cities: Lens[STATE, Seq[CITY]]
  def population: Lens[CITY, Double]

  /// Annual mean growth rate
  def rate: Double
  def stdRate: Double

  def growthRate(c: CITY)(implicit rng: Random) = 1 + (stdRate * rng.nextGaussian + rate)
}
