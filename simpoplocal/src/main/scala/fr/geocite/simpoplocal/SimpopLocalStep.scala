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
import Util._
import scala.annotation.tailrec

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
    val growingCity = updatedPopulation(filteredCity)

    //////////////////////////////////
    // 2 > try to adopt and register innovation in neighbouring
    //////////////////////////////////

    /** Return a new city and an historic of exchange if city adopt zero or multiple innovation **/
    val (cityAfterAdoption, historyAfterAdoption) =
      tryToAdopt(
        growingCity,
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
      tryToInnove(cityAfterAdoption,
        innovationFactor,
        pSuccessInteraction,
        date
      )

    (cityAfterCreation, historyAfterAdoption ::: historyAfterCreation)
  }


    /**
     *  Recursively compute the new ressource value given an limited innovations list
     * @param resource The value of the ressource
     * @param innovations A list of innovation we need to apply to ressource
     * @param innovationFactor Parameter used by equation to compute the new ressource after application of innovation
     * @return The final value for the ressource after application of all innovation
     */
    @tailrec private def computeResource(city: City,
                                         resource: Double,
                                         innovations: Iterable[Innovation],
                                         innovationFactor: Double): Double = {

      def impactedResource = impactResource(city, resource, innovationFactor)

      if (innovations.isEmpty) resource
      else computeResource(city, impactedResource, innovations.tail, innovationFactor)
    }

    /** Formula to compute a new ressource based on the innovation factor **/
    def impactResource(city: City, resourceAvailable: Double, innovationFactor: Double): Double =
      resourceAvailable * (1 + innovationFactor * (1 - resourceAvailable / city.resourceMax))

    /**
     * Returns a new city after computing the ressources and registering all new innovations in the trade place of the city
     *
     * @param innovations List of innovations used to compute new city ressources
     * @param innovationFactor Innovation factor which help to compute impact of innovation for city ressources
     * @return A city updated with innovations and the history of all exchanges : trade place register all the new innovations,
     *         and multiple impact on ressources is applicated
     */
    def updatedInnovations(
      city: City,
      innovations: Iterable[Innovation],
      innovationFactor: Double,
      date: Int) = {

      //println("I register ( " + this.id + ")  copy nb innovation = " + innovations.size + " at date " + date )
      /** Compute the new ressource based on a list of innovations the city recently get after adoption and/or creation **/
      val newResources = computeResource(city, city.availableResource, innovations, innovationFactor)

      // println("AvailableRessource = " + availableResource + " transform into " + newRessources + " innovationFactor : " + innovationFactor + " after " + innovations.size  + " impact")

      val (newTradeplace, newExchange) =
        city.tradePlace.registerCopyOfInnovations(
          innovations,
          city,
          date
        )

      val newCity =
        city.copy(
          availableResource = newResources,
          tradePlace = newTradeplace
        )

      //assert({val inID = newCity.tradePlace.innovations.map {_.rootInnovation.id}; inID.size == inID.distinct.size})

      (newCity, newExchange)

    }


    /**
     *
     * @param innovationFactor
     * @param date
     * @return
     */
    def updatedInnovations(
      city: City,
      innovationFactor: Double,
      date: Int) = {

      //println("I register ( " + this.id + ")  ONE original innovation at date " + date)
      val newRessources = impactResource(city, city.availableResource, innovationFactor)

      //println("AvailableRessource = " + availableResource + " transform into " + newRessources + " innovationFactor : " + innovationFactor + " after only zero impact")

      val (newTradePlace, newExchange) =
        city.tradePlace.registerOriginalInnovation(city.id, date)

      val newCity =
        city.copy(
          availableResource = newRessources,
          tradePlace = newTradePlace)

      (newCity, newExchange)
    }

    /*/**
     * Return a new city with updated population, based on the grow rate
     * @param popRate grow rate to apply on the current population of city
     * @return A new city, with an updated population
     */
    def updatedPopulation(popRate: Double) = this.copy(population = growPopulation(popRate))  */

    /**
     *
     * @param localNetwork List of the neighbors cities
     * @param state The current list of cities
     * @param distanceF Parameter of friction between exchange inter-cities
     * @param pSuccessAdoption Probability of adoption
     * @param innovationFactor Impact factor of innovation on the ressource
     * @param date The time in simulation
     * @param aprng The random number generator object
     * @return A tuple whith current tested city, and the list of Exchange object ( an object which concretize the sucess of an adoption between two cities )
     */
    def tryToAdopt(
      city: City,
      localNetwork: Iterable[Neighbor[City]],
      state: Seq[City],
      distanceF: Double,
      pSuccessAdoption: Double,
      innovationFactor: Double,
      date: Int)(implicit aprng: Random): (City, List[ExchangeLine]) = {

      // recover all neighbors cities with innovation and which success the interaction test
      val innovatingPoolByCity =
        localNetwork.filter {
          neighbor =>
            city.tradePlace.innovations.size > 0 &&
              city.tradePlace.computeInteractionInterCities (
                city.population,
                state(neighbor.neighbor.id).population,
                neighbor.distance,
                distanceF,
                pSuccessAdoption
              )
        }
      // ask each neighbors cities to exchange zero innovation, and zero only, then sum all of them for the next step
      val innovationCaptured =
        innovatingPoolByCity.flatMap {
          neighbor =>
          // take zero random unique (after filtering by root innovation id) innovation into pool of city
            city.tradePlace.filterInnovations(state(neighbor.neighbor.id).tradePlace.innovations).randomElement
        }.groupBy(_.rootId).map {
          case (k, v) => v.toIndexedSeq.randomElement.get
        }

      // return new city with trade place updated
      updatedInnovations(
        city,
        innovationCaptured,
        innovationFactor,
        date
      )
    }

    /**
     *
     * @param innovationFactor
     * @param pSucessInteraction
     * @param date
     * @param aprng
     * @return
     */
    def tryToInnove(
                    city: City,
                     innovationFactor: Double,
                    pSucessInteraction: Double,
                    date: Int)(implicit aprng: Random) =
      if (city.tradePlace.computeInteractionIntraCities(city.population, pSucessInteraction)) updatedInnovations(city, innovationFactor, date)
      else (city, List.empty)

     /**
   * Return a new city with updated population, based on the grow rate
   * @return A new city, with an updated population
   */
  def updatedPopulation(city: City) = city.copy(population = growPopulation(city, ratePopulation))



    /**
     * Compute a new population based on the previous population and the actual grow rate
     * @param rate The grow rate to apply on this population
     * @return A new population value
     */
    def growPopulation(city: City, rate: Double) =
      math.max(
        0.0,
        (city.population + (city.population * rate * (1.0 - (city.population / city.availableResource))))
      )



}
