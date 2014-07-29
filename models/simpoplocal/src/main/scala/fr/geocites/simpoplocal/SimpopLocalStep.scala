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

package fr.geocites.simpoplocal

import scala.util.Random
import scala.collection.mutable.ListBuffer
import scalaz._
import Scalaz._
import fr.geocites.simpuzzle.state.Step

trait SimpopLocalStep
    extends Step
    with SimpopLocalState
    with SimpopLocalInitialState
    with NoDisaster
    with InnovationRootIdOrdering
    with SimpopLocalLogging {

  /// The maximal annual growth on the settlements in inhabitants per step
  def populationRate: Double = 0.02

  /// The probability that an innovation emerges from the interaction between two individuals of the same settlement
  def pCreation: Double

  /// The probability that an innovation is transmitted between two individuals of different settlements
  def pDiffusion: Double

  /// The deterrent effect of the of distance on diffusion
  def distanceDecay: Double

  /// The impact of the acquisition of an innovation on the growth of a settlement
  def innovationImpact: Double

  /// The maximum carrying capacity of the landscape of each settlement (measured in number of inhabitants)
  def rMax: Double

  def nextState(state: STATE)(implicit rng: Random) = {
    val disasteredSettlements = disaster(state.settlements)

    var currentInnovationId = state.currentInnovationId
    val newSettlements = ListBuffer[Settlement]()
    val logs = ListBuffer[LOGGING]()

    for {
      settlement <- disasteredSettlements
    } {
      val evolved = evolveSettlement(settlement, disasteredSettlements, state.step, currentInnovationId)
      val (newSettlement, newId) = evolved.value
      currentInnovationId = newId
      newSettlements += newSettlement
      logs ++= evolved.written
    }

    log(SimpopLocalState(state.step + 1, settlements = newSettlements, currentInnovationId), logs)
  }

  /**
   *
   * Evolve a settlement (compute its new state).
   *
   * @param settlement The concerned settlement.
   * @param state Current state of the simulation.
   * @param step Current step of the simulation.
   * @return The new state of the settlement and the new value for next innovation id.
   */
  def evolveSettlement(settlement: Settlement, state: Seq[Settlement], step: Int, currentInnovationId: Int)(implicit rng: Random) = {
    val grownSettlement = growPopulation(settlement)
    diffusion(grownSettlement, state, step, currentInnovationId) flatMap {
      case (settlementAfterDiffusion, newInnovationId) => creation(settlementAfterDiffusion, state, step, newInnovationId)
    }
  }

  //By default no deprecation
  def filterObsolete(innovations: Iterable[Innovation], date: Int) = innovations

  /**
   *
   * Computes the process of diffusion of innovations for a settlement.
   *
   * @param settlement The concerned settlement.
   * @param state The current state of the simulation.
   * @param step The current step of the simulation.
   * @param nextInnovationId Next id available for innovation creation.
   * @param rng The random number generator.
   * @return The new state of the settlement after the diffusion process and the new value for next innovation id.
   */
  def diffusion(settlement: Settlement, state: Seq[Settlement], step: Int, nextInnovationId: Int)(implicit rng: Random) = {
    val localNetwork = network(settlement.id)

    // Recovers all neighbours settlements (and their innovations) which succeed in the diffusion process test.
    val innovationPoolByCity =
      localNetwork.filter {
        neighbour =>
          settlement.innovations.size > 0 && diffuse(state(settlement.id).population, state(neighbour.neighbour.id).population, neighbour.distance)
      }

    /**
     *
     * Filters the set of exchangeable innovations: the set of innovations from a neighbour that are not already present in the concerned settlement and not obsolete.
     *
     * @param from The settlement from which the innovation are diffused.
     * @return The set of exchangeable innovations.
     */
    def exchangeableInnovations(from: Settlement) = filterObsolete(from.innovations &~ settlement.innovations, step)

    case class ProposedExchange(neighbourId: Int, innovation: Innovation)

    // Randomly draws an innovation for each the neighbour selected for diffusion.
    val innovationsFromNeighbours =
      innovationPoolByCity.flatMap {
        neighbour =>
          val neighbourId = neighbour.neighbour.id
          val neighbourState = state(neighbourId)
          randomElement(exchangeableInnovations(neighbourState).toSeq).map(ProposedExchange(neighbourId, _))
      }

    // Randomly draws an innovation in case the same innovation (same rootId) is proposed by different neighbours.
    val distinctInnovations =
      innovationsFromNeighbours.groupBy(_.innovation.rootId).map {
        case (k, v) => randomElement(v).get
      }.toList

    // Copy the innovations acquired by diffusion.
    val copyOfInnovations =
      distinctInnovations.zipWithIndex.map {
        case (exchange, index) => Innovation(step = step, rootId = exchange.innovation.id, id = nextInnovationId + index)
      }

    val exchanges: List[LogInfo] = distinctInnovations.map { case ProposedExchange(n, i) => Diffused(n, settlement.id, i.rootId) }

    Writer(exchanges, (acquireInnovations(settlement, step, copyOfInnovations), nextInnovationId + copyOfInnovations.size))
  }

  /**
   *
   * Computes the process of creation of innovations for a settlement.
   *
   * @param settlement The concerned settlement.
   * @param state The current state of simulation.
   * @param step The current step of the simulation.
   * @param nextInnovationId Next id available for innovation creation.
   * @param rng The random number generator.
   * @return The new state of the settlement after the creation process and the new value for next innovation id.
   */
  def creation(settlement: Settlement, state: Seq[Settlement], step: Int, nextInnovationId: Int)(implicit rng: Random) =
    if (create(state(settlement.id).population)) {
      val innovation = Innovation(step = step, rootId = nextInnovationId, id = nextInnovationId)
      Writer(
        List[LogInfo](Created(settlement.id, innovation.rootId)),
        (acquireInnovations(settlement, step, Seq(innovation)), nextInnovationId + 1)
      )
    } else Writer(List.empty, (settlement, nextInnovationId))

  /**
   *
   * Randomly draws whether a creation of innovation occurs between two settlements.
   *
   * @param population1 The population of the first interacting settlement.
   * @param population2 The population of the second interacting settlement.
   * @param distance The distance between the two settlements.
   * @param rng The random number generator.
   * @return true if the diffusion occurs.
   */
  def diffuse(population1: Double, population2: Double, distance: Double)(implicit rng: Random) = {
    val population = population1 * population2
    val numberOfDistantInteractions = population / math.pow(distance, distanceDecay)
    val pBinomial = binomial(numberOfDistantInteractions, pDiffusion)
    rng.nextDouble <= pBinomial
  }

  /**
   *
   * Randomly draws whether a creation of innovation occurs in a settlement of a given population.
   *
   * @param population The population the settlement.
   * @param rng The random number generator.
   * @return true if the creation occurs.
   */
  def create(population: Double)(implicit rng: Random): Boolean = {
    val numberOfLocalInteractions = (1.0 / 2.0) * (population * (population - 1.0))
    val pBinomial = binomial(numberOfLocalInteractions, pCreation)
    rng.nextDouble <= pBinomial
  }

  /**
   *
   * Compute
   *
   * @param settlement The concerned settlement.
   * @return A new settlement state after evolution of it's population.
   */
  def growPopulation(settlement: Settlement) =
    settlement.copy(
      population =
        math.max(
          0.0,
          settlement.population + settlement.population * populationRate * (1.0 - settlement.population / settlement.availableResource)
        )
    )

  /**
   *
   * Computes the new state of a settlement after the acquisition of innovations. This method also impact the available resource of the settlement.
   *
   * @param settlement The concerned settlement.
   * @param step The current step of the simulation.
   * @param innovations The acquired innovations during the step.
   * @return The new state of the settlement
   */
  def acquireInnovations(settlement: Settlement, step: Int, innovations: Seq[Innovation]) =
    settlement.copy(
      availableResource = impactResource(settlement, innovations),
      innovations = settlement.innovations ++ innovations
    )

  /**
   *
   * Computes the effect of the acquisition of innovations on the available resource.
   *
   * @param settlement The concerned settlement.
   * @param innovations The acquired innovations.
   * @return The new amount of the available resource.
   */
  def impactResource(settlement: Settlement, innovations: Seq[Innovation]): Double =
    innovations.foldLeft(settlement.availableResource) {
      (resource, _) => resource * (1 + innovationImpact * (1 - resource / rMax))
    }

  /**
   *
   * Compute the probability of at least one success of a boolean random draw of probability of success p after n draws.
   *
   * @param n Number of draws.
   * @param p Probability of success of a single draw.
   * @return The probability of at least one success after n draws.
   */
  def binomial(n: Double, p: Double): Double = 1.0 - math.pow(1 - p, n)

  /**
   *
   * Draw a random element in a sequence.
   *
   * @param seq A sequence of elements.
   * @param rng The random number generator.
   * @tparam T The type of the elements.
   * @return A randomly drawn element.
   */
  def randomElement[T](seq: Seq[T])(implicit rng: Random) = if (seq.isEmpty) None else Some(seq(rng.nextInt(seq.size)))

}
