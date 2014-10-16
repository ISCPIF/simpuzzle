/*
 * Copyright (C) 16/09/13 Romain Reuillon
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

package fr.geocites.simpuzzle.logging

import scalaz.{ std, Writer }
import scalaz.Scalaz._
import util.{ Success, Failure }
import fr.geocites.simpuzzle.state

trait Logging <: state.State {
  type LOGGING

  def log[T](s: T, l: => List[LOGGING]) = s.set(l)

  protected implicit def tupleToWriter[T](t: (List[LOGGING], T)) = Writer(t._1, t._2)
  protected implicit def stateWriterToValidStateWriter(w: Writer[List[LOGGING], STATE]) = w.map(s => Success(s))
  protected implicit def toWriter[T](s: T): Writer[List[LOGGING], T] = log(s, List.empty)
  protected implicit def invalidStateToInvalidStateWriter(s: Failure[STATE]) = log(s, List.empty)

  protected implicit def listMonoid[T] = std.list.listMonoid[T]
}
