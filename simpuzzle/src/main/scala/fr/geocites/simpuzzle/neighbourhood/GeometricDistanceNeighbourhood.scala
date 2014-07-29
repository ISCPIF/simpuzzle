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

package fr.geocites.simpuzzle.neighbourhood

import fr.geocites.simpuzzle.city.{ Id, Radius, Position }
import fr.geocites.simpuzzle.distance.GeometricDistance

trait GeometricDistanceNeighbourhood <: GeometricDistance {
  case class Neighbor[T](neighbour: T, distance: Double)

  def neighbors[T <: Position with Radius with Id](all: Seq[T], center: T) =
    all.map {
      c => Neighbor(c, distance(center, c))
    }.filter {
      n => n.distance < center.radius && center.id != n.neighbour.id
    }

}
