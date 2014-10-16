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

import scala.collection.mutable.ListBuffer

object Network {

  def apply(outLinks: Seq[Seq[Int]]) =
    new Network {
      lazy val indexedOut = outLinks.map(_.toSet).toVector
      lazy val indexedIn = {
        val outs = Vector.fill(outLinks.size)(ListBuffer[Int]())
        for {
          (o, i) <- indexedOut.zipWithIndex
          j <- o
        } outs(j) += i
        outs.map(_.toSet)
      }

      override def outNodes(i: Int) = indexedOut(i).toVector
      override def inNodes(i: Int) = indexedIn(i).toVector

      override def mapNodes(f: (Int, Int) => Double) = {
        val matrix = SparseMatrix.builder(outLinks.size)

        for {
          (out, i) <- outLinks.zipWithIndex
          j <- out
        } matrix += (i, j, f(i, j))

        matrix.toMatrix
      }
    }

  def full(network: Int) =
    new Network {
      def allExcept(i: Int) = (0 until i) ++ (i + 1 until network)
      def outNodes(i: Int) = allExcept(i)
      def inNodes(i: Int) = allExcept(i)
      def mapNodes(f: (Int, Int) => Double): DenseMatrix =
        DenseMatrix(
          Array.tabulate(network, network) {
            (i, j) => if (i != j) f(i, j) else 0.0
          }
        )

    }

}

trait Network {
  def inNodes(c: Int): Seq[Int]
  def outNodes(c: Int): Seq[Int]
  def mapNodes(f: (Int, Int) => Double): Matrix
}
