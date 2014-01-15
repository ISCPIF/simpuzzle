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

  private lazy val a = 0.000292792792792793

  private lazy val b = 0.941441441441441

  private lazy val c = 0

  def initialWealth(population: Double)(implicit rng: Random): Double = a * pow(population, 2) + b * population + c

  def wealthToPopulation(wealth: Double) = {
    assert(a != 0)
    (-b +
      sqrt(pow(b, 2) - 4 * a * (c - wealth))
    ) / (2 * a)
  }
}
