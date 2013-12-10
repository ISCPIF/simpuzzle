/*
 * Copyright (C) 19/11/13 Romain Reuillon
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

package fr.geocite.marius.one

import util.Random
import math._

trait WealthFromPopulation {

  def inversionPoint: Double

  def minWealth: Double = 0

  def maxWealth: Double = 20000

  def minPopulation: Double = 10

  def maxPopulation: Double = 20000

  private lazy val den = 2 * inversionPoint * minPopulation + 2 * inversionPoint * maxPopulation - pow(minPopulation, 2) - pow(maxPopulation, 2)

  private lazy val a = {
    def aNum = minPopulation - maxPopulation - minWealth + maxWealth
    aNum / den
  }

  private lazy val b = {
    def bNum = 2 * inversionPoint * minWealth + 2 * inversionPoint * maxWealth - pow(minPopulation, 2) - pow(maxPopulation, 2)
    bNum / den
  }

  private lazy val c = {
    def cNum = -2 * inversionPoint * minPopulation * maxWealth + 2 * inversionPoint * maxPopulation * minWealth - pow(minPopulation, 2) * maxPopulation + pow(minPopulation, 2) * maxWealth + minPopulation * pow(maxPopulation, 2) - pow(maxPopulation, 2) * minWealth
    cNum / den
  }

  def initialWealth(population: Double)(implicit rng: Random): Double = a * pow(population, 2) + b * population + c

  def wealthToPopulation(wealth: Double) = {
    assert(a != 0)
    (-b +
      sqrt(pow(b, 2) - 4 * a * (c - wealth))
    ) / (2 * a)
  }
}
