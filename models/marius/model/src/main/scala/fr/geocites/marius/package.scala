/*
 * Copyright (C) 2015 Romain Reuillon
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
package fr.geocites

import fr.geocites.simpuzzle.Position

import scalaz._
import Scalaz._
import fr.geocites.simpuzzle.gis._

package object marius {

  import data._

  object DistanceMatrix {
    def apply[T: Position](positions: Seq[T]): DistanceMatrix = {
      val p = positions.toVector
      val distances = Array.ofDim[Double](p.size, p.size)

      for {
        i <- 0 until p.size
        j <- i until p.size
      } {
        if (i == j) distances(i)(i) = 0.0
        else {
          val d = geodedicDistance(p(i), p(j))
          distances(i)(j) = d
          distances(j)(i) = d
        }
      }
      distances
    }
  }

  type DistanceMatrix = Array[Array[Double]]

  implicit class IndexOption[T](s: Seq[T]) {
    def indexOption(t: T) =
      s.indexOf(t) match {
        case -1 => None
        case x => Some(x)
      }
  }
}
