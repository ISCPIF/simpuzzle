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


package fr.geocites.marius.behaviour

import fr.geocites.marius.Marius
import fr.geocites.marius.calibration.Evaluation.overflowRatio

import org.apache.commons.math3.fitting._
import java.util.{Vector => JVector}
import scala.util.{Random, Failure, Success, Try}

import monocle.syntax._

import fr.geocites.marius.{Marius, MariusFile}
import MariusFile._
import scala.util.{Random, Failure, Success, Try}
import math._
import scala.annotation.tailrec


object BehaviourComputing {
  def compute(marius: Marius)(implicit rng: Random): Seq[Double] = {
    import marius._

    def totalOverflowRatio(cities: Seq[CITY]) =
      cities.map {
        c =>
          overflowRatio(c |-> wealth get, marius.supply(c |-> population get)) +
            overflowRatio(c |-> wealth get, marius.demand(c |-> population get))
      }.sum

    def isValid(s: STATE): Boolean = {   
      val overflow = totalOverflowRatio(s |-> cities get)
      val deadCities = (s |-> cities get).count(c => (c |-> wealth get) <= 0.0)
      overflow == 0 || deadCities == 0
    }

    def toValidStatesOrNone(it: Iterator[Try[STATE]]): Option[List[STATE]] =
      if(it.hasNext)
        it.next match {
          case Success(s) =>
            toValidStatesOrNone(it) match {
              case Some(l) => Some(s :: l)
              case None => None
            }
          case Failure(_) => None
        }
      else Some(List[STATE]())

    // val (validstates, invalidstates) = marius.states.map(maybeS => maybeS match {
    //   case Success(s) => (s, isValid(s))
    //   case Failure(_) => (None, false)
    //   }) span {case (s, v) => v}

    val simres: Option[List[STATE]] = toValidStatesOrNone(marius.states)

    simres match {
      case None => Array[Double](-1e12, -1e12, -1e12)
      case Some(l) => {
        val seriestotalpop: Vector[Double] = (l map {s => ((s |-> cities get) map {c => c |-> population get}).sum}) toVector
        val lastpops: Vector[Double] = ((l.last |-> cities get) map {c => c |-> population get}) toVector
        val diffpopfinalinitial: Double = seriestotalpop.last - seriestotalpop.head
        Array[Double](slope(lastpops), diffpopfinalinitial, popincrementinversioncount(seriestotalpop))
      }
    }
  }

  def totalPopulation(populations: Seq[Double]) = populations.sum * 1000
  
  def totalWealth(wealths: Seq[Double]) = wealths.sum

  def slope(populations: Seq[Double]) = {
    val observedPoints = populations.map(_ * 1000).map(p => if (p < 1) 1 else p).sorted.reverse.zipWithIndex.map { case (p, i) => (math.log10(i.toDouble + 1), math.log10(p)) }
    val guessIntersect = observedPoints.head._2
    val guessSlope = (observedPoints.last._2 - observedPoints.head._2) / (observedPoints.last._1 - observedPoints.head._1)
    val init = Array(guessIntersect, guessSlope)
    val fitter = PolynomialCurveFitter.create(2).withMaxIterations(1000).withStartPoint(init)
    val wop = new JVector[WeightedObservedPoint](observedPoints.size)
    observedPoints foreach { case (x, y) => wop.add(new WeightedObservedPoint(1.0, x, y))}
    fitter.fit(wop)(1)
  }

  def popincrementinversioncount(populations: Seq[Double]) =
    if (populations.length <= 2) 0
    else {
      val increments: Seq[Double] = ((populations dropRight 1) zip (populations drop 1)) map {case (p1, p2) => p2 - p1}
      val inversioncount: Int = ((increments dropRight 1) zip (increments drop 1)).foldLeft[Int](0)({(count, inc) => if (inc._2.signum != inc._1.signum) count + 1 else count})
      inversioncount
    }
}
