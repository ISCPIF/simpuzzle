/*
 * Copyright (C) 25/04/13 Romain Reuillon
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

package fr.geocite.simpoplocal

import util.Random
import annotation.tailrec
import math._

object Util {
  implicit def indexedSeq2IndexedSeqDecorator[T](elts: IndexedSeq[T]) = new {
    def randomElement(implicit prng: Random) = if (elts.isEmpty) None else Some(elts(prng.nextInt(elts.size)))
  }

  def diffNonSorted[A](l: List[A], r: List[A])(implicit order: Ordering[A]) =
    diff(l.sorted(order), r.sorted(order))

  @tailrec def diff[A](l: List[A], r: List[A], acc: List[A] = List.empty)(implicit order: Ordering[A]): List[A] = {
    (l.headOption, r.headOption) match {
      case (None, None) => acc.reverse
      case (_, None) => l ::: (acc.reverse)
      case (None, _) => acc.reverse
      case (Some(el), Some(er)) =>
        if (order.equiv(el, er)) diff(l.tail, r, acc)
        else if (order.lt(el, er)) diff(l.tail, r, el :: acc)
        else diff(l, r.tail, acc)
    }
  }

  def dist(x: Double,
    y: Double,
    xOut: Double,
    yOut: Double): Double = sqrt(pow((x - xOut), 2) + pow((y - yOut), 2))

  def binomial(pool: Double,
    p: Double): Double = (1.0 - (pow((1 - p), pool)))

}
