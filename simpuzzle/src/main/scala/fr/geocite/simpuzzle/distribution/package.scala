/*
 * Copyright (C) 15/09/13 Romain Reuillon
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

package object distribution {

  implicit class StatisticDecorator[T <: Iterable[Double]](sequence: T) {

    def average = sequence.sum / sequence.size

    def mse = {
      val avg = sequence.average
      sequence.map { v ⇒ math.pow(v - avg, 2) }.average
    }

    def rmse = math.sqrt(sequence.mse)

    def centered = {
      val avg = sequence.average
      sequence.map(_ / avg)
    }

    def reduced = {
      val r = sequence.rmse
      sequence.map(_ / r)
    }

  }

  def multinomialDraw[T](s: Seq[(T, Double)])(implicit rng: Random) = {
    assert(!s.isEmpty, "Input sequence should not be empty")
    def select(remaining: List[(T, Double)], value: Double, begin: List[(T, Double)] = List.empty): (T, List[(T, Double)]) =
      remaining match {
        case (e, weight) :: tail =>
          if (value <= weight) (e, begin.reverse ::: tail)
          else select(tail, value - weight, (e, weight) :: begin)
        case _ => sys.error(s"Bug $remaining $value $begin")
      }
    val totalWeight = s.unzip._2.sum
    select(s.toList, rng.nextDouble * totalWeight)
  }
}
