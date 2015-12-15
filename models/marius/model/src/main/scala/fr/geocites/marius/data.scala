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
package fr.geocites.marius

object data {
  case class Cell(row: Int, value: Double)

  object Matrix {
    def apply(_content: Array[Array[Double]]) = new Matrix {
      override def content: Array[Array[Double]] = _content
    }
  }

  trait Matrix {
    def content: Array[Array[Double]]

    def side: Int = content.size
    def lines =
      content.view.map(_.view.zipWithIndex.map { case (v, i) => Cell(i, v) })

    def transpose: Matrix = Matrix(content.transpose)
    def linesContent: Vector[Vector[Double]] = content.map(_.toVector).toVector

    def map(f: (Int, Int, Double) => Double): Matrix = {
      val newContent = Array.ofDim[Double](side, side)
      for {
        i <- 0 until side
        j <- 0 until side
      } newContent(i)(j) = f(i, j, content(i)(j))
      Matrix(newContent)
    }

  }

  object Network {

    def full(network: Int) =
      new Network {
        def allExcept(i: Int) = (0 until i) ++ (i + 1 until network)
        def outNodes(i: Int) = allExcept(i).toVector
        def inNodes(i: Int) = allExcept(i).toVector
        def mapNodes(f: (Int, Int) => Double): Matrix = {
          val matrix = Array.ofDim[Double](network, network)
          for {
            i <- 0 until network
            j <- 0 until network
          } matrix(i)(j) = if (i != j) f(i, j) else 0.0
          Matrix(matrix)
        }

      }

  }

  trait Network {
    def inNodes(c: Int): Vector[Int]
    def outNodes(c: Int): Vector[Int]
    def mapNodes(f: (Int, Int) => Double): Matrix
  }

}
