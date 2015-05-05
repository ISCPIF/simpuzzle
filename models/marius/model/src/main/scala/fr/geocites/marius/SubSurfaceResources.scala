/*
 * Copyright (C) 2014 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.geocites.marius

import monocle._

trait SubSurfaceResources <: Marius {

  def oilAndGazEffect: Double
  def coalEffect: Double

  def oilOrGaz: Lens[CITY, Boolean]
  def coal: Lens[CITY, Boolean]

  override def resourcesEffect(cities: Seq[CITY], newWealths: Seq[Double]) =
    (cities zip newWealths).map {
      case (city, wealth) =>
        wealth * {
          1 +
            (if (oilOrGaz.get(city)) oilAndGazEffect else 0.0) +
            (if (coal.get(city)) coalEffect else 0.0)
        }
    }

}
