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

package fr.geocite.marius.matching

object SparseMatrix {
  case class Cell(row: Int, value: Double)

  def builder(_side: Int) =
    new SparseMatrixBuilder {
      override def side: Int = _side
    }

  def apply(cells: Seq[Seq[Cell]]) = new SparseMatrix {
    def lines: Seq[Seq[Cell]] = cells
  }
}

import SparseMatrix._
import scala.collection.mutable.ListBuffer

trait SparseMatrix {
  def side = lines.size
  def lines: Seq[Seq[Cell]]
  def transpose: SparseMatrix = {
    val builder = SparseMatrix.builder(side)
    for {
      (l, i) <- lines.zipWithIndex
      Cell(j, v) <- l
    } builder += (j, i, v)
    builder.toMatrix
  }
  def linesContent = lines.map(_.map(_.value))
}

trait SparseMatrixBuilder {
  def side: Int
  lazy val lines: Seq[ListBuffer[Cell]] = IndexedSeq.fill(side)(ListBuffer.empty)

  def +=(i: Int, j: Int, value: Double) {
    lines(i) += SparseMatrix.Cell(j, value)
  }

  def toMatrix = SparseMatrix(lines.map(_.toIndexedSeq))
}
