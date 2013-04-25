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

import State._
import math._

trait EuclideanNeighborhood {
  /**
   *  Compute the neigbors cities in a radius of one given city
   * @param cities list of cities used by function to compute neigbors
   * @param city the city, center of the circle for radius computation
   * @param radius size of the radius for the circular buffer around the city
   * @return a list of cities in the neighbors of the given city
   */
  def neighbors(cities: Seq[City], city: City, radius: Double) =
    cities.map {
      c => CityDist(c.id, dist(city.x, city.y, c.x, c.y))
    }.filter {
      cityDist => cityDist.distance < radius && cityDist.cityId != city.id
    }

  def dist(x: Double,
    y: Double,
    xOut: Double,
    yOut: Double): Double = sqrt(pow((x - xOut), 2) + pow((y - yOut), 2))

}
