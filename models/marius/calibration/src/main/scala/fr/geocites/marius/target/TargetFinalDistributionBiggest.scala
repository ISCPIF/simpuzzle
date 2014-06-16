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

package fr.geocites.marius.target

import fr.geocite.simpuzzle._
import statistics._

/*trait TargetFinalDistributionBiggest <: Target with Target.Final {
  def minPopulation: Int

  lazy val sortedEmpirical = finalPopulations.sorted.reverse
  lazy val biggest = sortedEmpirical.count(_ >= minPopulation)

  def target(d: Dynamic) = {
    val (biggestEmpirical, smallestEmpirical) = sortedEmpirical.splitAt(biggest)
    val (biggestSimulated, smallestSimulated) = d.last._2.sorted.reverse.splitAt(biggest)
    (logSquaresError(biggestEmpirical, biggestSimulated) / biggest) *
      (logSquaresError(smallestEmpirical, smallestSimulated) / (finalPopulations.size - biggest))
  }

}*/
