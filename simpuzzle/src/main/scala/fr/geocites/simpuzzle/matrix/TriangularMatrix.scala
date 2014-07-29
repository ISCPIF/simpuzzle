/*
 * Copyright (C) 04/02/14 Romain Reuillon
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

package fr.geocites.simpuzzle.matrix

import scala.reflect.ClassTag

object TriangularMatrix {

  def apply[@specialized(Double) T](c: Traversable[Traversable[T]], d: T)(implicit t: ClassTag[T]): TriangularMatrix[T] =
    apply(c.map(_.toArray).toArray, d)

  def apply[@specialized(Double) T](c: Array[Array[T]], d: T)(implicit t: ClassTag[T]): TriangularMatrix[T] =
    new TriangularMatrix[T] {
      val diagonal = d
      val content = c
      val tTag = implicitly[ClassTag[T]]
    }
}

trait TriangularMatrix[@specialized(Double) T] {

  def diagonal: T
  def content: Array[Array[T]]
  def side = content.size
  implicit val tTag: ClassTag[T]

  def apply(x: Int)(y: Int): T =
    if (x == y) diagonal
    else if (x < y) content(x)(y - x - 1) else content(y)(x - y - 1)

  def full: Array[Array[T]] = {
    val res = Array.ofDim[T](side, side)
    var i = 0
    while (i < side) {
      var j = 0
      while (j < side) {
        res(i)(j) = apply(i)(j)
        j += 1
      }
      i += 1
    }
    res
  }

}
