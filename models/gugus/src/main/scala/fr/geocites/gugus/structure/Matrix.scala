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

package fr.geocites.gugus.structure

object Matrix {
  def apply(_content: Array[Array[Double]]) = new Matrix {
    override def content: Array[Array[Double]] = _content
  }
}

trait Matrix {
  def content: Array[Array[Double]]

  def side: Int = content.size
  def lines: Seq[Seq[Cell]] =
    content.view.map(_.view.zipWithIndex.map { case (v, i) => Cell(i, v) })

  def transpose: Matrix = Matrix(content.transpose)
  def linesContent: Seq[Seq[Double]] = content.map(_.toIndexedSeq).toIndexedSeq

  def map(f: (Int, Int, Double) => Double): Matrix = {
    val newContent = Array.ofDim[Double](side, side)
    for {
      i <- 0 until side
      j <- 0 until side
    } newContent(i)(j) = f(i, j, content(i)(j))
    Matrix(newContent)
  }

}
