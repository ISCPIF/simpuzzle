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
import monocle.SimpleLens
import monocle.syntax._

trait UrbanTransition <: Gugus with UrbanisationFunction {

  type TERRITORY

  def territory: SimpleLens[CITY, String]
  def territories: SimpleLens[STATE, Seq[TERRITORY]]
  def urbanisationStep: SimpleLens[TERRITORY, Double]
  def territoryId: SimpleLens[TERRITORY, String]

  def ruralMultiplier: Double

  override def urbanTransition(state: STATE): STATE = {
    val territorialMultipliers =
      (for {
        territory <- (state |-> territories get)
        urbanisation = urbanisationFunction(territory |-> urbanisationStep get)
        id = (territory |-> territoryId get)
      } yield id -> (100.0 - urbanisation) * ruralMultiplier).toMap

    val newCities =
      for {
        c <- (state |-> cities get)
        t = (c |-> territory get)
        multiplier = territorialMultipliers(t)
      } yield c |-> population modify (_ * multiplier)

    val updatedTerritories = state |-> territories modify (_.map(t => t |-> urbanisationStep modify (_ + 1)))
    (updatedTerritories |-> cities set newCities)
  }

}
