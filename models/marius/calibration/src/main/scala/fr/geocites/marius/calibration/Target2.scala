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

package fr.geocites.marius.calibration

import fr.geocites.marius.{Marius, MariusFile}
import MariusFile._
import scala.util.{Random, Failure, Success, Try}
import math._
import monocle.syntax._

object Target {

  def logSquaresError(d1: Seq[Double], d2: Seq[Double]) =
    (d1 zip d2) map {
      case (e, o) =>
        pow(log10(o) - log10(e), 2)
    } sum

  def date(step: Int) = MariusFile.dates.head + step

  def distanceToData(step: Int, populations: Seq[Double]) =
    MariusFile.populations(dates.head + step).map {
      empirical => logSquaresError(populations.sorted, empirical.sorted)
    }.getOrElse(0.0)

  def sum(it: Iterator[Seq[Double]]) = it.foldLeft(Seq(0.0, 0.0, 0.0)) { (s, v) => (s zip v).map { case (x, y) => x + y } }

}

import Target._


object Target1 {

  def error(marius: Marius)(implicit rng: Random): Double =
    Try {
      import marius._
      val fitness =
        (for { (state, step) <- marius.states.zipWithIndex } yield state match {
          case Success(s) => distanceToData(step, (s |-> marius.cities get) map (_ |-> population get))
          case Failure(_) => Double.PositiveInfinity
        }).sum
      if (fitness.isNaN) Double.PositiveInfinity else fitness
    }.getOrElse(Double.PositiveInfinity)
}


object Target2 {

  def error(marius: Marius)(implicit rng: Random): Array[Double] = Try {

    import marius._

    def totalOverflowRatio(cities: Seq[CITY]) =
      cities.map {
        c =>
          overflowRatio(c |-> wealth get, marius.supply(c |-> population get)) +
            overflowRatio(c |-> wealth get, marius.demand(c |-> population get))
      }.sum

    def overflowRatio(wealth: Double, flow: Double) = {
      val ratio = flow / wealth
      if (ratio < 1.0) 0.0 else ratio - 1
    }

    val fitness =
      sum(
        for { (state, step) <- marius.states.zipWithIndex } yield {
          state match {
            case Success(s) =>
              val overflow = totalOverflowRatio(s |-> marius.cities get)
              val deadCities = (s |-> marius.cities get).count(c => (c |-> wealth get) <= 0.0)
              val distance = distanceToData(step, (s |-> cities get) map (_ |-> population get))
              Seq(deadCities, distance, overflow)
            case Failure(_) =>
              Seq(Double.PositiveInfinity, Double.PositiveInfinity, Double.PositiveInfinity)
          }
        }
      )

    fitness.map(x => if (x.isNaN) Double.PositiveInfinity else x)
  }.getOrElse(Seq(Double.PositiveInfinity, Double.PositiveInfinity, Double.PositiveInfinity)).toArray

}
