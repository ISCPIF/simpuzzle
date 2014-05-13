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

    def median: Double = {
      val sortedSerie = sequence.toArray.filterNot(_.isNaN).sorted
      val size = sortedSerie.size
      if (size == sequence.size)
        if (size % 2 == 0) (sortedSerie(size / 2) + sortedSerie((size / 2) - 1)) / 2
        else sortedSerie((size / 2))
      else Double.NaN
    }

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

    def normalise = {
      val max = sequence.max
      val min = sequence.min
      sequence.map(v => (v - min) / (max - min))
    }

  }

  def multinomialDraw[T](s: Seq[(Double, T)])(implicit rng: Random) = {
    assert(!s.isEmpty, "Input sequence should not be empty")
    def select(remaining: List[(Double, T)], value: Double, begin: List[(Double, T)] = List.empty): (T, List[(Double, T)]) =
      remaining match {
        case (weight, e) :: tail =>
          if (value <= weight) (e, begin.reverse ::: tail)
          else select(tail, value - weight, (weight, e) :: begin)
        case _ => sys.error(s"Bug $remaining $value $begin")
      }
    val totalWeight = s.unzip._1.sum
    select(s.toList, rng.nextDouble * totalWeight)
  }
}
