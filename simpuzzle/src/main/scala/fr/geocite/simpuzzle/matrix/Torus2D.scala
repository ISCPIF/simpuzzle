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

package fr.geocite.simpuzzle.matrix

trait Torus2D extends Matrix2D {
  def positiveMod(i: Int, j: Int) = {
    val m = i % j
    if (m < 0) m + j else m
  }

  override def cell(x: Int, y: Int) = {
    val xSize = cells.size
    val row = cells(positiveMod(x, xSize))
    row(positiveMod(y, row.size))
  }

  def cellsIndices =
    cells.zipWithIndex.flatMap {
      case (l, i) => l.zipWithIndex.map { case (c, j) => ((i, j), c) }
    }

}
