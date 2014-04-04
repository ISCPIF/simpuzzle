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

package fr.geocite.simpuzzle.logging

import scalaz._
import Scalaz._
import fr.geocite.simpuzzle.state

trait Logging <: state.State {
  type LOGGING

  def log[T](s: T, l: Seq[LOGGING]) = s.set(l)

  protected implicit def tupleToWriter[T](t: (Seq[LOGGING], T)) = Writer(t._1, t._2)
  protected implicit def stateWriterToValidStateWriter(w: Writer[Seq[LOGGING], STATE]) = w.map(s => ValidState(s))
  protected implicit def toWriter[T](s: T): Writer[Seq[LOGGING], T] = log(s, Seq.empty)
  protected implicit def invalidStateToInvalidStateWriter(s: InvalidState) = log(s, Seq.empty)
}
