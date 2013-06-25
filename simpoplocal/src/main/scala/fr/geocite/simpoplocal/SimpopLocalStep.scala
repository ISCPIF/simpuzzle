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
import scala.annotation.tailrec

trait SimpopLocalStep extends fr.geocite.simpuzzle.Step with SimpopLocalState with SimpopLocalInitialState with NoDisaster {

  def distanceDecay: Double

  def pDiffusion: Double

  def pCreation: Double

  def innovationImpact: Double

  def populationRate = 0.02

  def step(state: STATE)(implicit rng: Random) = {
    val disasteredCities = disaster(state.cities)

    val (newCities, newId) =
      disasteredCities.foldLeft(List.empty[City] -> state.currentInnovationId) {
        (acc, city) =>
          val (newCities, id) = acc
          val (newCity, newId) = evolveCity(city.id, disasteredCities, state.date, id)
          (newCity :: newCities, newId)
      }

    SimpopLocalState(state.date + 1, cities = newCities.reverse, newId)
  }

  /**
   *
   * @param cityId Id of city
   * @param state Indexed List of city object, position of city is based on the cityId parameter
   * @param date date of evolution
   * @return a list of tuple which associate a city with a list of ExchangeLine (history of exchange).
   */
  def evolveCity(cityId: Int, state: Seq[City], date: Int, currentInnovationId: Int)(implicit rng: Random) = {
    val city = state(cityId)
    val filteredCity = deprecateInnovations(city, date)

    /** Return a new city after evolution of it's population  **/
    val growingCity = growPopulation(filteredCity)

    /** Return a new city after the diffusion **/
    val (cityAfterDiffusion, newInnovationId) = diffuse(growingCity, state, date, currentInnovationId)

    /** Return a new city and an historic of exchange if city create a new innovation **/
    create(cityAfterDiffusion, date, newInnovationId)
  }

  //By default no deprecation
  def deprecateInnovations(city: City, date: Int): City = city

  /**
   *
   * @param state The current list of cities
   * @param date The time in simulation
   * @param rng The random number generator object
   * @return A tuple with current tested city, and the list of Exchange object ( an object which concretize the sucess of an adoption between two cities )
   */
  def diffuse(city: City, state: Seq[City], date: Int, currentInnovationId: Int)(implicit rng: Random) = {
    val localNetwork = territory(city.id)

    // recover all neighbors cities with innovation and which success the interaction test
    val innovatingPoolByCity =
      localNetwork.filter {
        neighbor =>
          city.innovations.size > 0 && diffusion(city.population, state(neighbor.neighbor.id).population, neighbor.distance)
      }

    val innovationsFromNeighbours =
      innovatingPoolByCity.flatMap {
        neighbor => exchangeableInnovations(city, state(neighbor.neighbor.id)).randomElement
      }

    val capturedInnovations =
      innovationsFromNeighbours.groupBy(_.rootId).map {
        case (k, v) => v.randomElement.get
      }.toList

    val copyOfInnovation =
      capturedInnovations.zipWithIndex.map {
        case (innovation, index) => new Innovation(city = city.id, date = date, rootId = innovation.id, id = currentInnovationId + index)
      }

    (addInnovations(city, date, copyOfInnovation), currentInnovationId + copyOfInnovation.size)
  }

  def create(city: City, date: Int, currentInnovationId: Int)(implicit rng: Random) =
    if (creation(city.population)) {
      val innovation = new Innovation(city = city.id, date = date, rootId = currentInnovationId, id = currentInnovationId)
      (addInnovations(city, date, List(innovation)), currentInnovationId + 1)
    } else (city, currentInnovationId)

  def diffusion(popCityStart: Double, popCityEnd: Double, distance: Double)(implicit rng: Random) = {
    val population = popCityStart * popCityEnd
    val formula = population / math.pow(distance, distanceDecay)
    val pCopyInnovation = rng.nextDouble
    val pBinomial = binomial(formula, pDiffusion)
    pCopyInnovation <= pBinomial
  }

  def creation(popCity: Double)(implicit aprng: Random): Boolean = {
    val formula = (1.0 / 2.0) * (popCity * (popCity - 1.0))
    val pCreateInnovation = aprng.nextDouble
    val pBinomial = binomial(formula, pCreation)
    pCreateInnovation <= pBinomial
  }

  /**
   * Return a new city with updated population, based on the grow rate
   * @return A new city, with an updated population
   */
  def growPopulation(city: City) =
    city.copy(
      population =
        math.max(
          0.0,
          city.population + city.population * populationRate * (1.0 - city.population / city.availableResource)

        )
    )

  /**
   * Returns a new city after computing the resources and registering all new innovations in the trade place of the city
   *
   */
  def addInnovations(city: City, date: Int, innovations: List[Innovation]) =
    city.copy(availableResource = impactResource(city, innovations), innovations = innovations ::: city.sortedInnovations)

  def impactResource(city: City, innovations: List[Innovation]): Double =
    innovations.foldLeft(city.availableResource) {
      (resource, _) => impactResource(city, resource)
    }

  /** Formula to compute a new resource based on the innovation factor **/
  def impactResource(city: City, resourceAvailable: Double): Double =
    resourceAvailable * (1 + innovationImpact * (1 - resourceAvailable / rMax))

  def exchangeableInnovations(from: City, to: City) = diff(from.sortedInnovations, to.sortedInnovations)

  def binomial(pool: Double, p: Double): Double = 1.0 - math.pow(1 - p, pool)

  implicit class IndexedSeqDecorator[T](elts: Seq[T]) {
    def randomElement(implicit prng: Random) = if (elts.isEmpty) None else Some(elts(prng.nextInt(elts.size)))
  }

  @tailrec final def diff[A](l: List[A], r: List[A], acc: List[A] = List.empty)(implicit order: Ordering[A]): List[A] = {
    (l.headOption, r.headOption) match {
      case (None, None) => acc.reverse
      case (_, None) => l ::: acc.reverse
      case (None, _) => acc.reverse
      case (Some(el), Some(er)) =>
        if (order.equiv(el, er)) diff(l.tail, r, acc)
        else if (order.lt(el, er)) diff(l.tail, r, el :: acc)
        else diff(l, r.tail, acc)
    }
  }

}
