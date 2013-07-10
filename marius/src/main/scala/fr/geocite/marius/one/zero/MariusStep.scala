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

import fr.geocite.simpuzzle._
import scala.util.Random

trait MariusStep <: Step with MariusState {

  def adjustConsumption: Double

  def adjustProductivity: Double

  def territorialTaxes: Double

  def capitalShareOfTaxes: Double

  def distanceDecay: Double

  /**
   * Relative importance of distance in the choice of partners in according to the interaction potential
   * distanceOrderBuy gives more importance to Distance to select potential buyers
   * distanceOrderSell gives more importance to Supply to select potential sellers
   * so distanceOrderBuy should be higher than distanceOrderSell
   */
  def distanceOrderBuy: Double

  def distanceOrderSell: Double

  def partnerMultiplier: Double

  def conversionFactor: Double

  def step(s: STATE)(implicit rng: Random) = {

    //TODO update wealth (budget supply)
    val supplies = s.cities.map(c => supply(c.population, c.wealth))
    val demands = s.cities.map(c => demand(c.population))
    val potentialBuyers: Seq[Set[Int]] = potentialBuyerNetwork(s.cities, s.distanceMatrix, supplies, demands)
    val potentialSellers: Seq[Set[Int]] = potentialSellerNetwork(s.cities, s.distanceMatrix, supplies, demands)

    val sellContracts: Seq[Set[Int]] = contracts(potentialBuyers, potentialSellers)
    val buyContract: Seq[Set[Int]] = contracts(potentialSellers, potentialBuyers)

    val propositionToSell: Seq[Map[Int, Double]] =
      (sellContracts zip supplies).map {
        case (buyers, supply) => reservations(buyers.toSeq, demands, supply)
      }

    val propositionToBuy: Seq[Map[Int, Double]] =
      (buyContract zip demands).map {
        case (sellers, demand) => reservations(sellers.toSeq, supplies, demand)
      }

    case class Transaction(from: Int, to: Int, pps: Double, ppb: Double) {
      def transacted = math.min(pps, ppb)
      def delta = ppb - pps
    }

    def computeTransactions(from: Int, ppss: Map[Int, Double]): Seq[Transaction] =
      ppss.toSeq.map {
        case (to, pps) =>
          val ppb = propositionToBuy(to).getOrElse(from, sys.error(s"Transaction is empty $from to $to"))
          Transaction(from, to, pps, ppb)
      }

    val transactionsForCities: Seq[Transaction] =
      propositionToSell.zipWithIndex.flatMap {
        case (ppss, city) => computeTransactions(city, ppss)
      }

    val transactedFrom =
      transactionsForCities.groupBy(_.from).map {
        case (f, transactions) => f -> (transactions.map(_.transacted).sum, transactions.map(_.delta).sum)
      }.withDefaultValue((0.0, 0.0))

    val transactedTo =
      transactionsForCities.groupBy(_.to).map {
        case (t, transactions) => t -> (transactions.map(_.transacted).sum, transactions.map(_.delta).sum)
      }.withDefaultValue((0.0, 0.0))

    val commercialBalance =
      s.cities.zipWithIndex.map {
        case (c, i) => transactedFrom(i)._1 - transactedTo(i)._1
      }

    val tBalance = territoryBalance(s.cities)

    def aboveOne(v: Double) = if (v <= 1) 1.0 else v

    val wealths =
      (s.cities.map(_.wealth) zip commercialBalance zip tBalance).map {
        case ((wealth, cb), tb) =>
          wealth + cb + tb
      }

    val opportunities =
      (s.cities.zipWithIndex zip commercialBalance zip tBalance).map {
        case (((c, i), cb), tb) => transactedFrom(i)._2 + tb + cb
      }

    val populations =
      (s.cities.map(_.population) zip opportunities).map {
        case (p, op) =>
          op match {
            case 0 => p
            case x if x > 0 => p + math.log(op / conversionFactor)
            case x if x < 0 => p - math.log(math.abs(op) / conversionFactor)
          }
      }

    val newCities =
      (s.cities zip populations zip wealths).map {
        case ((c, p), w) =>
          c.copy(population = aboveOne(p), wealth = aboveOne(w))
      }

    s.copy(step = s.step + 1, cities = newCities)
  }

  def contracts(from: Seq[Set[Int]], to: Seq[Set[Int]]) =
    from.zipWithIndex.map {
      case (pTo, city) =>
        pTo.filter { to(_).contains(city) }
    }

  def reservations(requester: Seq[Int], requested: Seq[Double], toShare: Double) = {
    val effectiveRequests = requester.map(requested)
    val totalRequest = effectiveRequests.sum
    val fractions = effectiveRequests.map(_ / totalRequest)
    (requester zip fractions.map(_ * toShare)).toMap
  }

  def matchNetwork(potentialBuyers: Seq[Int], potentialSellers: Seq[Int]) =
    potentialBuyers.toSet & potentialSellers.toSet

  def consumption(population: Double) = adjustConsumption * math.log(population)

  def productivity(wealth: Double) = adjustProductivity * math.log(wealth)

  def demand(population: Double) = consumption(population) * population

  def supply(population: Double, wealth: Double) = productivity(wealth) * population

  def potentialBuyerNetwork(
    s: Seq[City],
    distances: DistanceMatrix,
    supplies: Seq[Double],
    demands: Seq[Double])(implicit rng: Random) = {
    val acquaintance = acquaintanceNetwork(s, supplies, distances, distanceOrderBuy)
    (s zip demands).zipWithIndex.map {
      case ((c1, d1), i) =>
        drawCandidates(acquaintance(i), distances, supplies, s => s >= partnerMultiplier * d1)
    }
  }

  def potentialSellerNetwork(
    s: Seq[City],
    distances: DistanceMatrix,
    supplies: Seq[Double],
    demands: Seq[Double])(implicit rng: Random) = {
    val acquaintance = acquaintanceNetwork(s, supplies, distances, distanceOrderSell)
    (s zip supplies).zipWithIndex.map {
      case ((c1, s1), i) =>
        drawCandidates(acquaintance(i), distances, supplies, s => s >= partnerMultiplier * s1)
    }
  }

  def drawCandidates(
    weighted: Seq[(Int, Double)],
    distances: DistanceMatrix,
    otherQuantity: Seq[Double],
    satisfied: Double => Boolean)(implicit rng: Random) = {
    def drawOneCandidate(candidates: List[(Int, Double)], selected: List[Int] = List.empty, totalQuantity: Double = 0): Set[Int] =
      if (candidates.isEmpty || satisfied(totalQuantity)) selected.toSet
      else {
        val (s, remaining) = multinomialDraw(candidates)
        drawOneCandidate(remaining, s :: selected, totalQuantity + otherQuantity(s))
      }
    drawOneCandidate(weighted.toList).toSet
  }

  def acquaintanceNetwork(s: Seq[City], supplies: Seq[Double], distances: DistanceMatrix, beta: Double) = {
    val citiesWithSupply = s zip supplies
    citiesWithSupply.zipWithIndex.toIndexedSeq.map {
      case ((c1, s1), i) =>
        citiesWithSupply.zipWithIndex.flatMap {
          case ((c2, s2), j) =>
            if (i == j) None
            else Some(j -> interactionPotential(s1, s2, distances(i)(j), beta))
        }
    }
  }

  def interactionPotential(supply1: Double, supply2: Double, distance: Double, beta: Double) = {
    val potential = math.pow(supply1 * supply2, 1 - beta) / math.pow(math.pow(distance, distanceDecay), beta)
    assert(potential >= 0, s"Error in potential computing gave $potential for $supply1 $supply2 $distance $beta")
    potential
  }

  def territoryBalance(s: Seq[City]): Seq[Double] = {
    val deltas =
      for {
        (r, cs) <- s.zipWithIndex.groupBy(_._1.region)
        (cities, indexes) = cs.unzip
      } yield {
        val taxes = cities.map(_.wealth * territorialTaxes)
        val capitalShare = capitalShareOfTaxes * taxes.sum
        val taxesLeft = taxes.sum - capitalShare
        val regionPopulation = cities.map(_.population).sum

        val territorialDeltas = (cities zip taxes).map {
          case (city, cityTaxes) =>
            val populationShare = city.population / regionPopulation

            val delta =
              (if (city.capital) taxesLeft * populationShare + capitalShare
              else taxesLeft * populationShare) - cityTaxes
            delta
        }
        indexes zip territorialDeltas
      }

    deltas.flatten.toSeq.sortBy {
      case (i, _) => i
    }.unzip._2
  }

  def multinomialDraw[T](s: Seq[(T, Double)])(implicit rng: Random) = {
    assert(!s.isEmpty, "Input sequence should not be empty")
    def select(remaining: List[(T, Double)], value: Double, begin: List[(T, Double)] = List.empty): (T, List[(T, Double)]) =
      remaining match {
        case (e, weight) :: tail =>
          if (value <= weight) (e, begin.reverse ::: tail)
          else select(tail, value - weight, (e, weight) :: begin)
        case _ => sys.error(s"Bug $remaining $value $begin")
      }
    val totalWeight = s.unzip._2.sum
    select(s.toList, rng.nextDouble * totalWeight)
  }

}
