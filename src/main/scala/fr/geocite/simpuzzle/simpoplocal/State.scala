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

package fr.geocite.simpuzzle.simpoplocal

import util.Random
import java.util.concurrent.atomic.AtomicInteger
import Util._
import scala.annotation.tailrec
import fr.geocite.simpuzzle.neighborhood._

object State {

  case class City(
      id: Int,
      x: Double,
      y: Double,
      population: Double,
      availableResource: Double,
      resourceMax: Double,
      percolationIndex: Int,
      cityClass: Int,
      tradePlace: TradePlace) extends Position with Radius with Id {

    def rangeRadiusClass1 = 20.0
    def rangeRadiusClass2 = 10.0
    def rangeRadiusClass3 = 5.0

    def radius =
      cityClass match {
        case 1 => rangeRadiusClass1
        case 2 => rangeRadiusClass2
        case 3 => rangeRadiusClass3
        case _ => error(s"Invalid city class $cityClass")
      }

    /**
     *  Recursively compute the new ressource value given an limited innovations list
     * @param resource The value of the ressource
     * @param innovations A list of innovation we need to apply to ressource
     * @param innovationFactor Parameter used by equation to compute the new ressource after application of innovation
     * @return The final value for the ressource after application of all innovation
     */
    @tailrec private def computeResource(resource: Double,
      innovations: Iterable[Innovation],
      innovationFactor: Double): Double = {

      def impactedResource = impactResource(resource, innovationFactor)

      if (innovations.isEmpty) resource
      else computeResource(impactedResource, innovations.tail, innovationFactor)
    }

    /** Formula to compute a new ressource based on the innovation factor **/
    def impactResource(resourceAvailable: Double, innovationFactor: Double): Double = resourceAvailable * (1 + innovationFactor * (1 - resourceAvailable / resourceMax))

    /**
     * Returns a new city after computing the ressources and registering all new innovations in the trade place of the city
     *
     * @param innovations List of innovations used to compute new city ressources
     * @param innovationFactor Innovation factor which help to compute impact of innovation for city ressources
     * @return A city updated with innovations and the history of all exchanges : trade place register all the new innovations,
     *         and multiple impact on ressources is applicated
     */
    def updatedInnovations(innovations: Iterable[Innovation],
      innovationFactor: Double,
      date: Int) = {

      //println("I register ( " + this.id + ")  copy nb innovation = " + innovations.size + " at date " + date )
      /** Compute the new ressource based on a list of innovations the city recently get after adoption and/or creation **/
      val newResources = computeResource(availableResource, innovations, innovationFactor)

      // println("AvailableRessource = " + availableResource + " transform into " + newRessources + " innovationFactor : " + innovationFactor + " after " + innovations.size  + " impact")

      val (newTradeplace, newExchange) =
        tradePlace.registerCopyOfInnovations(
          innovations,
          this,
          date
        )

      val newCity =
        this.copy(
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
    def updatedInnovations(innovationFactor: Double,
      date: Int) = {

      //println("I register ( " + this.id + ")  ONE original innovation at date " + date)
      val newRessources = impactResource(availableResource, innovationFactor)

      //println("AvailableRessource = " + availableResource + " transform into " + newRessources + " innovationFactor : " + innovationFactor + " after only one impact")

      val (newTradePlace, newExchange) =
        tradePlace.registerOriginalInnovation(this.id, date)

      val newCity =
        this.copy(
          availableResource = newRessources,
          tradePlace = newTradePlace)

      (newCity, newExchange)
    }

    /**
     * Return a new city with updated population, based on the grow rate
     * @param popRate grow rate to apply on the current population of city
     * @return A new city, with an updated population
     */
    def updatedPopulation(popRate: Double) = this.copy(population = growPopulation(popRate))

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
      localNetwork: Iterable[DistanceNeighborhood.Neighbor[City]],
      state: Seq[City],
      distanceF: Double,
      pSuccessAdoption: Double,
      innovationFactor: Double,
      date: Int)(implicit aprng: Random): (City, List[ExchangeLine]) = {

      // recover all neighbors cities with innovation and which success the interaction test
      val innovatingPoolByCity =
        localNetwork.filter {
          neighbor =>
            tradePlace.innovations.size > 0 &&
              tradePlace.computeInteractionInterCities(
                population,
                state(neighbor.neighbor.id).population,
                neighbor.distance,
                distanceF,
                pSuccessAdoption
              )
        }
      // ask each neighbors cities to exchange one innovation, and one only, then sum all of them for the next step
      val innovationCaptured =
        innovatingPoolByCity.flatMap {
          neighbor =>
            // take one random unique (after filtering by root innovation id) innovation into pool of city
            tradePlace.filterInnovations(state(neighbor.neighbor.id).tradePlace.innovations).randomElement
        }.groupBy(_.rootId).map {
          case (k, v) => v.toIndexedSeq.randomElement.get
        }

      // return new city with trade place updated
      updatedInnovations(
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
    def tryToInnove(innovationFactor: Double,
      pSucessInteraction: Double,
      date: Int)(implicit aprng: Random) = {

      if (tradePlace.computeInteractionIntraCities(population, pSucessInteraction))
        updatedInnovations(innovationFactor, date)
      else (this, List.empty)
    }

    /**
     * Compute a new population based on the previous population and the actual grow rate
     * @param rate The grow rate to apply on this population
     * @return A new population value
     */
    def growPopulation(rate: Double) =
      math.max(
        0.0,
        (population + (population * rate * (1.0 - (population / availableResource))))
      )

  }

  object TradePlace {

    def apply(
      innovations: List[Innovation] = List.empty,
      countCreatedInnovation: Int = 0,
      countAcquiredInnovation: Int = 0) = new TradePlace(innovations.sorted(Innovation.orderByRootId), countCreatedInnovation, countAcquiredInnovation)
  }

  /**
   *
   * @param innovations
   * @param countCreatedInnovation
   * @param countAcquiredInnovation
   */
  class TradePlace private (
      val innovations: List[Innovation] = List.empty,
      val countCreatedInnovation: Int = 0,
      val countAcquiredInnovation: Int = 0) {

    def copy(
      innovations: List[Innovation] = this.innovations,
      countCreatedInnovation: Int = this.countCreatedInnovation,
      countAcquiredInnovation: Int = this.countAcquiredInnovation) = TradePlace(innovations, countCreatedInnovation, countAcquiredInnovation)

    /** Return the total innovation in the trade place : adopt + created **/
    def totalInnovation = countCreatedInnovation + countAcquiredInnovation

    /**
     * Dyssimetric diff between this two pool of innovation (mine and the list in the param), we want to remove all equal fatherInnovation
     * @param innovationsAvailable
     * @return
     */
    def filterInnovations(innovationsAvailable: List[Innovation]) =
      diff(innovationsAvailable, innovations).toIndexedSeq

    /**
     * Create and register one innovation by turn
     * @param city
     * @param time
     * @return
     */
    def registerOriginalInnovation(
      city: Int,
      time: Int) = {

      val innovation = new Innovation(city = city, date = time)

      val newTradePlace =
        this.copy(
          innovation :: innovations,
          countCreatedInnovation + 1,
          countAcquiredInnovation
        )

      val newHistory = List(ExchangeLine(time, city, city, innovation))
      (newTradePlace, newHistory)
    }

    /**
     * Duplicate and Register multiple copy of innovation
     * @param newInnovations
     * @param city
     * @param time
     * @return
     */
    def registerCopyOfInnovations(
      newInnovations: Iterable[Innovation],
      city: City,
      time: Int) = {

      // Need to recopy innovation object to store hierarchical diffusion
      // ( before, after )
      val copyOfInnovation =
        newInnovations.toList.map {
          innovation =>
            (innovation, innovation.cloneWith(city = city.id, date = time))
        }

      val newHistory =
        copyOfInnovation.map {
          case (before, after) => ExchangeLine(time, before.city, after.city, after)
        }

      val newTradePlace =
        this.copy(
          copyOfInnovation.map {
            case (before, after) => after
          } ::: this.innovations,
          countCreatedInnovation,
          countAcquiredInnovation + newInnovations.size
        )

      (newTradePlace, newHistory)
    }

    /**
     *
     * @param popCityStart
     * @param popCityEnd
     * @param distance
     * @param distanceF
     * @param pSuccessAdoption
     * @param aprng
     * @return
     */
    def computeInteractionInterCities(popCityStart: Double,
      popCityEnd: Double,
      distance: Double,
      distanceF: Double,
      pSuccessAdoption: Double)(implicit aprng: Random) = {

      val population = (popCityStart * popCityEnd)
      val formula = (population / math.pow(distance, distanceF))
      val pCopyInnovation = aprng.nextDouble

      val pBinomial = binomial(formula, pSuccessAdoption)

      pCopyInnovation <= pBinomial
    }

    /**
     *
     * @param popCity
     * @param pSuccessInteraction
     * @param aprng
     * @return
     */
    def computeInteractionIntraCities(popCity: Double,
      pSuccessInteraction: Double)(implicit aprng: Random): Boolean = {

      val formula = ((1.0 / 2.0) * (popCity * (popCity - 1.0)))
      val pCreateInnovation = aprng.nextDouble

      val pBinomial = binomial(formula, pSuccessInteraction)

      pCreateInnovation <= pBinomial
    }

  }

  object Innovation {
    implicit val orderByRootId = Ordering.by((_: Innovation).rootId)
    val curId = new AtomicInteger
  }

  class Innovation(
      val city: Int,
      val date: Int,
      _rootId: Option[Int] = None,
      val id: Int = Innovation.curId.getAndIncrement) {

    val rootId = _rootId.getOrElse(id)

    def cloneWith(
      city: Int = this.city,
      date: Int = this.date) = {

      new Innovation(city,
        date,
        Some(rootId)
      )
    }

    override def toString = "Id=" + id + ", RootId=" + rootId
  }

  case class ExchangeLine(date: Int, cityStart: Int, cityEnd: Int, innovation: Innovation)

  case class SimpopLocalState(date: Int, cities: Seq[City])

  case class CityDist(cityId: Int, distance: Double)

}

trait State extends fr.geocite.simpuzzle.State {
  type STATE = State.SimpopLocalState
}
