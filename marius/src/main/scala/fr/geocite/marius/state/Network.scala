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

package fr.geocite.marius.state

object Network {

  def apply(outLinks: Seq[Seq[Int]]) =
    new Network {
      lazy val indexedOut = outLinks.map(_.toSet)
      override def outNode(i: Int) = outLinks(i)
      override def existsOut(from: Int, to: Int): Boolean = outNode(from).contains(to)
    }

  def full(nodes: Seq[Int]) =
    new Network {
      override def outNode(c: Int): Iterable[Int] = nodes.slice(0, c) ++ nodes.slice(c + 1, nodes.size)
      override def existsOut(from: Int, to: Int): Boolean = true
    }

}

trait Network {
  def outNode(c: Int): Iterable[Int]
  def existsOut(from: Int, to: Int): Boolean
}
