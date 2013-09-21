/*
 * Copyright (C) 23/04/13 Romain Reuillon
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

package fr.geocite.simpuzzle

import scala.util.Random
import scalaz.Writer

trait StepByStep <: State with InitialState with Step with EndingCondition {

  def states(implicit rng: Random): Iterator[Writer[Seq[LOGGING], STATE]] =
    Iterator.iterate(initial) {
      s => step(s.value)
    }.takeWhile(s => !ended(s.value))

  def run(implicit rng: Random) = {
    def last[T](i: Iterator[T]): T = {
      val e = i.next
      if (i.hasNext) last(i)
      else e
    }
    last(states)
  }

}
