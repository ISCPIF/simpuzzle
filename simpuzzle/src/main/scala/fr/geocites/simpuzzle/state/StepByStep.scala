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

package fr.geocites.simpuzzle.state

import fr.geocites.simpuzzle.logging._
import scala.util.Random
import scalaz.Writer
import util.{ Success, Failure, Try }
import fr.geocites.simpuzzle.extendIterator

trait StepByStep <: State with InitialState with Step with EndingCondition with Logging {

  def logs(implicit rng: Random): Iterator[Writer[List[LOGGING], Try[STATE]]] =
    Iterator.iterate[Writer[List[LOGGING], Try[STATE]]](tryValid(initialState)) {
      v =>
        v.value match {
          case Success(s) => tryValid { nextState(s) }
          case s: Failure[_] => v
        }
    }.takeWhileInclusive {
      _.value match {
        case Success(s) => !ended(s)
        case _: Failure[_] => false
      }
    }

  def validStatesLogs(implicit rng: Random): Iterator[Writer[List[LOGGING], STATE]] =
    logs.collect {
      case x if x.value.isInstanceOf[Success[_]] =>
        val util.Success(state) = x.value.asInstanceOf[Success[STATE]]
        log(state, x.written)
    }

  def states(implicit rng: Random) =
    logs.map { _.value }

  def validStates(implicit rng: Random): Iterator[STATE] =
    states.collect { case Success(x) => x }

  def run(implicit rng: Random) = states.last

  private def tryValid(f: => Writer[List[LOGGING], STATE]): Writer[List[LOGGING], Try[STATE]] =
    try f.map(Success(_))
    catch {
      case e: AssertionError => Failure(e)
    }
}
