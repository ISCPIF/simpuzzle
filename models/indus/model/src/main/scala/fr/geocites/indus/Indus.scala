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

package fr.geocites.indus

import fr.geocites.gugus.Gugus
import fr.geocites.gugus.balance.{ InitialWealth, SuperLinearInitialWealth }
import fr.geocites.simpuzzle.flatten

import scala.util.Random

case class City(
  population: Double,
  wealth: Double,
  district: String,
  state: String,
  districtCapital: Boolean,
  stateCapital: Boolean)

trait Indus <: Gugus with SuperLinearInitialWealth {

  def initialCities(implicit rng: Random) = {
    val initialPopulations = IndusFile.initialPopulations
    val pop = initialPopulations.toSeq
    val initialWealths = InitialWealth.rescaleWealth(pop.map(initialWealth), pop)

    val cities =
      for {
        (_population,
          _district,
          _state,
          _districtCapital,
          _stateCapital,
          _initialWealth) <- pop zip IndusFile.districts zip IndusFile.states zip IndusFile.districtCapitals zip IndusFile.stateCapitals zip initialWealths map (flatten)
      } yield {
        City(
          population = _population,
          district = _district,
          state = _state,
          districtCapital = _districtCapital,
          stateCapital = _stateCapital,
          wealth = _initialWealth
        )
      }
    cities.take(IndusFile.nbCities).toVector
  }

}
