/*
 * Copyright (C) 25/04/13 Romain Reuillon
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

package fr.geocite.simpuzzle.simpoplocal

trait EndingCondition extends fr.geocite.simpuzzle.EndingCondition with State {

  def maxInnovation: Double

  /**
   * @param cities list of cities used to compute this indicator
   * @return sum of population for this list of cities
   */
  def maxCities(state: STATE): Double =
    state.cities.map {
      _.population
    }.max

  /**
   * @param cities list of cities used to compute this indicator
   * @return sum of all innovation stored in multiple tradeplaces/cities
   */
  def maxInnovation(state: STATE): Double =
    state.cities.map {
      _.tradePlace.totalInnovation
    }.sum

  def ended(state: STATE) = {
    val maxInnov = maxInnovation(state)
    val maxCity = maxCities(state)

    // 3a - Break the simulation loop if one of these conditions is true
    state.date >= 4000 || /*maxPop > 70.0 || */ maxInnov > maxInnovation
  }
}