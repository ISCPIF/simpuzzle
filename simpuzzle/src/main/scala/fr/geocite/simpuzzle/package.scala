/*
 * Copyright (C) 14/05/13 Romain Reuillon
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

package fr.geocite

import scala.util.Random
import org.apache.commons.math3.random.{ Well44497a, RandomAdaptor }

package object simpuzzle {

  def random(seed: Long) = new util.Random(new RandomAdaptor(new Well44497a(seed)))

  object rng { implicit lazy val defaultRng = new Random }

  // Extends iterator :
  // based on answer here http://stackoverflow.com/questions/9329876/scala-extending-the-iterator
  class IteratorExtension[A](i: Iterator[A]) {
    def takeWhileInclusive(p: A => Boolean) = {
      val (a, b) = i.span(p)
      a ++ (if (b.hasNext) Some(b.next) else None)
    }
  }

  implicit def extendIterator[A](i: Iterator[A]) = new IteratorExtension(i)

  import shapeless._
  import ops.tuple.FlatMapper
  import syntax.std.tuple._

  def typed[T](t: => T) {}

  trait LowPriorityFlatten extends Poly1 {
    implicit def default[T] = at[T](Tuple1(_))
  }
  object flatten extends LowPriorityFlatten {
    implicit def caseTuple[P <: Product](implicit fm: FlatMapper[P, flatten.type]) =
      at[P](_.flatMap(flatten))
  }
}

