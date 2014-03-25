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

  def logs(implicit rng: Random): Iterator[Writer[Seq[LOGGING], GenericState]] =
    Iterator.iterate(initialState) {
      v =>
        v.value match {
          case ValidState(s) => nextState(s)
          case s: InvalidState => v
        }
    }.takeWhileInclusive {
      _.value match {
        case ValidState(s) => !ended(s)
        case _: InvalidState => false
      }
    }

  def validStatesLogs(implicit rng: Random): Iterator[Writer[Seq[LOGGING], STATE]] =
    logs.collect {
      case x if x.value.isInstanceOf[ValidState] =>
        val casted: ValidState = x.value.asInstanceOf[ValidState]
        log(casted.state, x.written)
    }

  def states(implicit rng: Random) =
    logs.map { _.value }

  def validStates(implicit rng: Random): Iterator[STATE] =
    states.collect { case ValidState(x) => x }

  def run(implicit rng: Random) = states.last
}
