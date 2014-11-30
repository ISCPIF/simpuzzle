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

package fr.geocites.gugus.balance

import scala.util.Random

object InitialWealth {

  def rescaleWealth(wealth: Seq[Double], population: Seq[Double]) = {
    val factor = population.sum / wealth.sum
    wealth.map(_ * factor)
  }

}

trait InitialWealth {
  def initialWealth(population: Double)(implicit rng: Random): Double
}

trait SuperLinearInitialWealth <: InitialWealth {

  def populationToWealthExponent: Double

  def initialWealth(population: Double)(implicit rng: Random): Double =
    math.pow(population, populationToWealthExponent)
}