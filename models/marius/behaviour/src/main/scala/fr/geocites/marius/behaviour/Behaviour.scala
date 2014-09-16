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

import org.apache.commons.math3.fitting._
import java.util.{Vector => JVector}

object Behaviour {

  def totalPopulation(populations: Seq[Double]) = populations.sum * 1000
  
  def totalWealth(wealths: Seq[Double]) = wealths.sum

  def slope(populations: Seq[Double]) = {
    val observedPoints = populations.map(_ * 1000).map(p => if (p < 1) 1 else p).sorted.reverse.zipWithIndex.map { case (p, i) => (math.log10(i.toDouble + 1), math.log10(p)) }
    val guessIntersect = observedPoints.head._2
    val guessSlope = (observedPoints.last._2 - observedPoints.head._2) / (observedPoints.last._1 - observedPoints.head._1)
    val guessCurve = 0
    val init = Array(guessIntersect, guessSlope, guessCurve)
    val fitter = PolynomialCurveFitter.create(3).withMaxIterations(1000).withStartPoint(init)
    val wop = new JVector[WeightedObservedPoint](observedPoints.size)
    observedPoints foreach { case (x, y) => wop.add(new WeightedObservedPoint(1.0, x, y)) }
    val Array(a, _) = fitter.fit(wop)
    a
  }
}
