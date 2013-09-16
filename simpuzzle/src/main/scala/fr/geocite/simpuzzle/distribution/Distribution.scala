/*
 * Copyright (C) 17/05/13 Romain Reuillon
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

package fr.geocite.simpuzzle.distribution

import scala.util.Random

object Distribution {

  def apply[T](s: Iterator[T]) = new Distribution[T] {
    def apply(implicit rng: Random) = s
  }

  trait IterableDistribution[T] extends Distribution[T] {
    def iterable: Iterable[T]
    def size = iterable.size
    def apply(implicit rng: Random) = iterable.toIterator
  }

  def apply[T](s: Iterable[T]) = new IterableDistribution[T] with Finite[T] {
    def iterable = s
  }
}

trait Distribution[T] {
  def apply(implicit rng: Random): Iterator[T]
  def iterator(implicit rng: Random) = apply
}
