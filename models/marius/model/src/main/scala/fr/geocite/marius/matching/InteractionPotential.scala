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

package fr.geocite.marius.matching

import fr.geocite.simpuzzle._

trait InteractionPotential {

  def distanceDecay: Double

  def interactionPotential(mass1: Double, mass2: Double, distance: Double) = {
    val potential = (mass1 * mass2) / math.pow(distance, distanceDecay)
    check(potential >= 0, s"Error in potential computing gave $potential for $mass1 $mass2 $distance")
    potential
  }
}