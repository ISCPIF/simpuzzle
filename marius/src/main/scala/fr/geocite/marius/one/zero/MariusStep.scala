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
import distribution._
import scala.util.Random

import scalaz._
import Scalaz._

trait MariusStep <: Step
    with MariusState
    with MariusLogging {

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

  def wealthSavingRate: Double = 0.15

  def fixedCost: Double = 42

  def internalShare: Double = 0.20

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
        case (buyers, supply) =>
          reservations(buyers.toSeq, demands, supply)
      }

    val propositionToBuy: Seq[Map[Int, Double]] =
      (buyContract zip demands).map {
        case (sellers, demand) =>
          reservations(sellers.toSeq, supplies, demand)
      }

    def computeTransactions(from: Int, ppss: Map[Int, Double]): Seq[Transaction] =
      ppss.toSeq.map {
        case (to, pps) =>
          val ppb = propositionToBuy(to).getOrElse(from, sys.error(s"Transaction is empty $from to $to"))
          assert(!ppb.isNaN)
          assert(!pps.isNaN)
          Transaction(from, to, pps, ppb)
      }

    val transactionsForCities: Seq[Transaction] =
      propositionToSell.zipWithIndex.flatMap {
        case (ppss, city) => computeTransactions(city, ppss)
      }

    val transactedFrom: Map[Int, Seq[Transaction]] =
      transactionsForCities.groupBy(_.from).withDefaultValue(Seq.empty)

    val transactedTo: Map[Int, Seq[Transaction]] =
      transactionsForCities.groupBy(_.to).withDefaultValue(Seq.empty)

    val tBalance = territoryBalance(s.cities)

    def aboveOne(v: Double) = if (v <= 1) 1.0 else v

    def wealths: Seq[Double] = {
      def bonuses = {
        val allTransactionsDist =
          for {
            cid <- 0 until s.cities.size
            tfrom = transactedFrom(cid)
            tto = transactedTo(cid)
          } yield tfrom ++ tto

        def nbDist = allTransactionsDist.map(_.size.toDouble).centered.reduced
        def nbTotal = allTransactionsDist.map(_.map(_.transacted).sum).centered.reduced
        (nbDist zip nbTotal).map { case (x, y) => x + y }
      }

      def unsolds =
        for {
          cid <- 0 until s.cities.size
          transaction = transactedFrom(cid)
        } yield transaction.map(_.pps).sum - transaction.map(_.transacted).sum

      def unsatisfieds =
        for {
          cid <- 0 until s.cities.size
          transaction = transactedTo(cid)
        } yield transaction.map(_.ppb).sum - transaction.map(_.transacted).sum

      (s.cities zip supplies zip demands zip unsolds zip unsatisfieds zip bonuses zip tBalance).map(flatten).map {
        case (city, supply, demand, unsold, unsatified, bonus, tb) =>
          city.wealth + supply - internalShare * demand * 2 + demand - fixedCost + bonus - unsold + unsatified
      }
    }

    def populations = {
      /*val commercialBalance =
   s.cities.zipWithIndex.map {
     case (c, i) =>
       transactedFrom(i).map(_.transacted).sum -
         transactedTo(i).map(_.transacted).sum
   }*/

      val wealthAdjustement =
        (wealths, tBalance).zipped.map {
          case (newWealth, tb) =>
            assert(!tb.isNaN)
            tb + newWealth
        }

      (s.cities.map(_.population) zip wealthAdjustement).map {
        case (p, wa) =>
          assert(!p.isNaN)
          assert(!wa.isNaN)
          wa match {
            case 0 => p
            case x if x > 0 => p + math.log(wa / conversionFactor)
            case x if x < 0 => p - math.log(math.abs(wa) / conversionFactor)
          }
      }
    }

    def savings =
      s.cities.map(_.wealth * wealthSavingRate)

    val newCities =
      (s.cities zip populations zip wealths zip savings).map(flatten).map {
        case (c, p, w, s) =>
          assert(p >= 0)
          assert(w > 0, s"The city too poor for the model $w, $p")
          c.copy(population = p, wealth = aboveOne(w), saving = s)
      }

    s.copy(step = s.step + 1, cities = newCities).set(transactionsForCities)
  }

  def contracts(from: Seq[Set[Int]], to: Seq[Set[Int]]) =
    from.zipWithIndex.map {
      case (pTo, city) =>
        pTo.filter { to(_).contains(city) }
    }

  def reservations(requester: Seq[Int], requested: Seq[Double], toShare: Double): Map[Int, Double] = {
    val effectiveRequests = requester.map(requested)
    val totalRequest = effectiveRequests.sum
    if (totalRequest <= 0) Map.empty
    else {
      val fractions = effectiveRequests.map(_ / totalRequest)
      (requester zip fractions.map(_ * toShare)).toMap
    }
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
    val interactionPotentials = interactionPotentialNetwork(s, supplies, distances, distanceOrderBuy)
    (s zip demands).zipWithIndex.map {
      case ((c1, d1), i) =>
        drawCandidates(interactionPotentials(i), distances, supplies, s => s >= partnerMultiplier * d1)
    }
  }

  def potentialSellerNetwork(
    s: Seq[City],
    distances: DistanceMatrix,
    supplies: Seq[Double],
    demands: Seq[Double])(implicit rng: Random) = {
    val acquaintance = interactionPotentialNetwork(s, supplies, distances, distanceOrderSell)
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
    drawOneCandidate(weighted.toList)
  }

  def interactionPotentialNetwork(s: Seq[City], supplies: Seq[Double], distances: DistanceMatrix, beta: Double) = {
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

}
