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

package fr.geocite.simpuzzle.state

import scala.util.Random
import scalaz._
import fr.geocite.simpuzzle.extendIterator

trait StepByStep <: State with InitialState with Step with EndingCondition {

  def logs(implicit rng: Random): Iterator[Writer[Seq[LOGGING], STATE]] =
    Iterator.iterate(initialState) {
      _.value match {
        case s: VALID_STATE => nextState(s)
        case x => x
      }
    }.takeWhileInclusive {
      _.value match {
        case s: VALID_STATE => ended(s)
        case _ => false
      }
    }

  def validStatesLogs(implicit rng: Random) = logs.collect { case x: Writer[_, VALID_STATE] => x }

  def states(implicit rng: Random) =
    logs.map { _.value }

  def validStates(implicit rng: Random) =
    states.collect { case x: VALID_STATE => x }

  def run(implicit rng: Random) = states.last
}
