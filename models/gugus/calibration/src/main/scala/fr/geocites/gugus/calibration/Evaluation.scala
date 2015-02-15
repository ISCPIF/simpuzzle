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

package fr.geocites.gugus.calibration

import monocle.syntax._

import scala.util.{ Failure, Success, Try, Random }
import math._

object Evaluation {

  def apply(m: Evaluable) = new Evaluation {
    override val model: Evaluable = m
  }

  def overflowRatio(wealth: Double, flow: Double) = {
    val ratio = flow / wealth
    if (ratio < 1.0) 0.0 else ratio - 1
  }
}

trait Model {
  val model: Evaluable
}

trait Overflow <: Model {
  import model._

  def totalOverflowRatio(cities: Seq[CITY]) =
    cities.map {
      c =>
        Evaluation.overflowRatio(c |-> wealth get, supply(c |-> population get)) +
          Evaluation.overflowRatio(c |-> wealth get, demand(c |-> population get))
    }.sum
}

trait Evaluation <: Overflow {

  import model._

  def singleMacro(implicit rng: Random): Double =
    Try {
      val fitness =
        (for { (state, step) <- states.zipWithIndex } yield state match {
          case Success(s) => distanceToData(step, (s |-> cities get) map (_ |-> population get), _.sorted)
          case Failure(_) => Double.PositiveInfinity
        }).sum
      if (fitness.isNaN) Double.PositiveInfinity else fitness
    }.getOrElse(Double.PositiveInfinity)

  def multiMacro(implicit rng: Random) = multi(distanceToData(_, _, _.sorted))
  def multiMicro(implicit rng: Random) = multi(distanceToData(_, _, identity))

  private def date(step: Int) = firstDate + step

  private def distanceToData(step: Int, simulated: Seq[Double], preProcessing: Seq[Double] => Seq[Double]) =
    populations(date(step)).map {
      empirical => logSquaresError(preProcessing(simulated), preProcessing(empirical))
    }.getOrElse(0.0)

  private def multi(distanceFunction: (Int, Seq[Double]) => Double)(implicit rng: Random): Array[Double] = Try {

    val fitness =
      sum(
        for { (state, step) <- states.zipWithIndex } yield {
          state match {
            case Success(s) =>
              val cs = s |-> cities get
              val overflow = totalOverflowRatio(cs)
              val deadCities = (cs).count(c => (c |-> wealth get) <= 0.0)
              val distance = distanceFunction(step, cs map (_ |-> population get))
              val eval = Seq(deadCities.toDouble, distance, overflow)
              eval.map(_ / steps / cs.size)
            case Failure(_) =>
              Seq(Double.PositiveInfinity, Double.PositiveInfinity, Double.PositiveInfinity)
          }
        }
      )

    fitness.map(x => if (x.isNaN) Double.PositiveInfinity else x)
  }.getOrElse(Seq(Double.PositiveInfinity, Double.PositiveInfinity, Double.PositiveInfinity)).toArray

  private def logSquaresError(d1: Seq[Double], d2: Seq[Double]) =
    (d1 zip d2) map {
      case (e, o) =>
        pow(log10(o) - log10(e), 2)
    } sum

  private def sum(it: Iterator[Seq[Double]]) = it.foldLeft(Seq(0.0, 0.0, 0.0)) { (s, v) => (s zip v).map { case (x, y) => x + y } }

}
