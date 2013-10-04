/*
 * Copyright (C) 27/06/13 Romain Reuillon
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

package fr.geocite.marius.one.zero

import fr.geocite.simpuzzle.State
import fr.geocite.simpuzzle.city.Population

trait MariusState <: State {
  case class City(population: Double, wealth: Double, region: String, capital: Boolean, saving: Double) extends Population
  type DistanceMatrix = Seq[Seq[Double]]
  case class MariusState(step: Int, cities: Seq[City], distanceMatrix: DistanceMatrix)
  type STATE = MariusState
}
