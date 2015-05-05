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

package fr.geocites.gugus.urbanisation

import fr.geocites.gugus.Gugus
import monocle._

trait UrbanTransition <: Gugus with UrbanisationFunction {

  type TERRITORY

  def territory: Lens[CITY, String]
  def territories: Lens[STATE, Seq[TERRITORY]]
  def urbanisationStep: Lens[TERRITORY, Double]
  def territoryId: Lens[TERRITORY, String]

  def ruralMultiplier: Double

  override def urbanTransition(state: STATE): STATE = {
    val territorialMultipliers =
      (for {
        territory <- territories.get(state)
        urbanisation = urbanisationFunction(urbanisationStep.get(territory))
        id = (territoryId.get(territory))
      } yield id -> (1.0 + (1.0 - urbanisation) * ruralMultiplier)).toMap

    val newCities =
      for {
        c <- cities.get(state)
        t = territory.get(c)
        multiplier = territorialMultipliers(t)
      } yield population.modify(_ * multiplier)(c)

    val updatedTerritories = territories.modify(_.map(urbanisationStep.modify(_ + 1)))(state)
    cities.set(newCities)(updatedTerritories)
  }

}
