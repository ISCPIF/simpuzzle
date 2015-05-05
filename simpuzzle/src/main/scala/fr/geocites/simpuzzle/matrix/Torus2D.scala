/*
 * Copyright (C) 09/09/13 Romain Reuillon
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

object Torus2D {
  def apply[T](cs: Seq[Seq[T]]) =
    new Torus2D[T] {
      override def cells = cs
    }
}

trait Torus2D[CELL] extends Matrix2D[CELL] {

  def positiveMod(i: Int, j: Int) = {
    val m = i % j
    if (m < 0) m + j else m
  }

  override def cell(x: Int, y: Int) = {
    val row = cells(positiveMod(x, cells.size))
    row(positiveMod(y, row.size))
  }

  def cellsIndices =
    cells.zipWithIndex.flatMap {
      case (l, i) => l.zipWithIndex.map { case (c, j) => ((i, j), c) }
    }

}
