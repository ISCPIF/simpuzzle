/*
 * Copyright (C) 25/04/13 Romain Reuillon
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

package fr.geocite.simpoplocal

import scala.util.Random

trait SimpopLocalStep extends fr.geocite.simpuzzle.Step with SimpopLocalState with SimpopLocalInitialState {

  def distanceF: Double
  def pSuccessAdoption: Double
  def pSuccessInteraction: Double
  def innovationFactor: Double

  def ratePopulation = 0.02

  def step(state: STATE)(implicit rng: Random) = {
    // 3b - Apply on each city the function evolveCity() which contain the sequence of action for each city
    // and return a list of tuple (city, exchange) which represent a new state at time t+1 for these cities
    val evolved =
      state.cities.map {
        city => evolveCity(city.id, state.cities, state.date)
      }

    // 4 - take only the first object (cities) in the list "evolved"
    SimpopLocalState(state.date + 1, cities = evolved.map { case (city, _) => city })
  }

  //By default no deprecation
  def deprecateInnovations(city: City, date: Int): City = city

  /**
   * *
   *
   * @param cityId Id of city
   * @param state Indexed List of city object, position of city is based on the cityId parameter
   * @param date date of evolution
   * @return a list of tuple which associate a city with a list of ExchangeLine (history of exchange).
   */
  def evolveCity(
    cityId: Int,
    state: Seq[City],
    date: Int)(implicit rng: Random): (City, List[ExchangeLine]) = {

    //////////////////////////////////
    // 0 > Update state of tradePlace
    //////////////////////////////////

    // Get the current city object (position equal to cityId in the list of city state)
    val city: City = state(cityId)

    // Filter city, recreate trade place with only innovation life , we remove all innovation which have life equal to 0
    val filteredCity = deprecateInnovations(city, date)

    //////////////////////////////////
    // 1 > Growing cities
    //////////////////////////////////

    /** Return a new city after evolution of it's population  **/
    val growingCity = filteredCity.updatedPopulation(ratePopulation)

    //////////////////////////////////
    // 2 > try to adopt and register innovation in neighbouring
    //////////////////////////////////

    /** Return a new city and an historic of exchange if city adopt zero or multiple innovation **/
    val (cityAfterAdoption, historyAfterAdoption) =
      growingCity.tryToAdopt(
        territory(cityId),
        state,
        distanceF,
        pSuccessAdoption,
        innovationFactor,
        date
      )

    //////////////////////////////////
    // 4 > try to create new innovation i, current city
    //////////////////////////////////

    /**Return a new city and an historic of exchange if city create a new innovation **/
    val (cityAfterCreation, historyAfterCreation) =
      cityAfterAdoption.tryToInnove(
        innovationFactor,
        pSuccessInteraction,
        date
      )

    (cityAfterCreation, historyAfterAdoption ::: historyAfterCreation)
  }

}
