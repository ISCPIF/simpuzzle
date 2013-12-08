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

package fr.geocite.simpuzzle

import scalaz._
import Scalaz._

trait Logging <: State {
  type LOGGING

  def log[T](s: T, l: Seq[LOGGING]) = s.set(l)

  protected implicit def tupleToWriter[T](t: (Seq[LOGGING], T)) = Writer(t._1, t._2)
  protected implicit def stateToWriter(s: STATE): Writer[Seq[LOGGING], STATE] = log(s, Seq.empty)
}
