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

package fr.geocite.marius.structure

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
      override def existsOut(from: Int, to: Int): Boolean = indexedOut(from).contains(to)
      override def inNodes(i: Int) = indexedIn(i).toVector
      override def existsIn(from: Int, to: Int) = indexedIn(from).contains(to)

      override def map(f: (Int, Int) => Double) = {
        val matrix = SparseMatrix.builder(outLinks.size)

        for {
          (out, i) <- outLinks.zipWithIndex
          j <- out
        } matrix += (i, j, f(i, j))

        matrix.toMatrix
      }
    }

  def full(nodes: Seq[Int]) =
    new Network {
      def allExcept(c: Int) = nodes.slice(0, c) ++ nodes.slice(c + 1, nodes.size)

      override def outNodes(c: Int) = allExcept(c)
      override def existsOut(from: Int, to: Int): Boolean = true
      override def inNodes(c: Int) = allExcept(c)
      override def existsIn(from: Int, to: Int) = true
      override def map(f: (Int, Int) => Double) = {
        DenseMatrix(Array.tabulate(nodes.size, nodes.size) { (i, j) =>
          if (i != j) f(i, j) else 0.0
        })
      }

    }

}

trait Network {
  def inNodes(c: Int): Seq[Int]
  def existsIn(from: Int, to: Int): Boolean
  def outNodes(c: Int): Seq[Int]
  def existsOut(from: Int, to: Int): Boolean
  def map(f: (Int, Int) => Double): Matrix
}
