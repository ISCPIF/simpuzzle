/*
 * Copyright (C) 03/02/14 Romain Reuillon
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

package fr.geocite.marius

import scala.util.Random
import scala.math._

trait WealthFromPopulation {

  private lazy val a = coeffA(popMin, popMax, wMin, wMax, inversionPoint)

  private lazy val b = coeffB(popMin, popMax, wMin, wMax, inversionPoint)

  private lazy val c = 0

  def popMin: Double
  def popMax: Double
  def inversionPoint: Double
  def wMin: Double
  def wMax: Double

  def initialWealth(population: Double)(implicit rng: Random): Double = a * pow(population, 2) + b * population + c

  def wealthToPopulation(wealth: Double) = {
    assert(a != 0)
    (-b +
      sqrt(pow(b, 2) - 4 * a * (c - wealth))
    ) / (2 * a)
  }

  def denominator(popMin: Double, popMax: Double, inversionPoint: Double): Double =
    2 * inversionPoint * popMin - 2 * inversionPoint * popMax - pow(popMin, 2) + pow(popMax, 2)

  def coeffA(popMin: Double, popMax: Double, wMin: Double, wMax: Double, inversionPoint: Double): Double =
    (popMin - popMax - wMin + wMax) / denominator(popMin, popMax, inversionPoint)

  def coeffB(popMin: Double, popMax: Double, wMin: Double, wMax: Double, inversionPoint: Double): Double =
    (2 * inversionPoint * wMin - 2 * inversionPoint * wMax - pow(popMin, 2) + pow(popMax, 2)) / denominator(popMin, popMax, inversionPoint)

}
