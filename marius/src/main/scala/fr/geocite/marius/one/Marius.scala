/*
 * Copyright (C) 20/11/13 Romain Reuillon
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

package fr.geocite.marius.one

import scala.util.Random
import scalaz._
import Scalaz._
import fr.geocite.simpuzzle._
import distribution._
import fr.geocite.marius.one.matching.Matching
import fr.geocite.marius._
import fr.geocite.gis.distance.GeodeticDistance

trait Marius <: StepByStep
    with MariusLogging
    with Matching
    with MariusState
    with MariusFile
    with WealthFromPopulation
    with PositionDistribution
    with GeodeticDistance {

  type CITY <: City
  def copy(c: CITY)(population: Double = c.population, wealth: Double = c.wealth, saving: Double = c.saving): CITY

  def adjustConsumption: Double

  def adjustProductivity: Double

  def territorialTaxes: Double

  def capitalShareOfTaxes: Double

  def wealthSavingRate: Double = 0.15

  def fixedCost: Double = 42

  def internalShare: Double = 0.20

  def step(s: STATE)(implicit rng: Random) = {

    def aboveOne(v: Double) = if (v <= 1) 1.0 else v
    val tBalance = territoryBalance(s.cities)

    for {
      wealths <- wealths(s, tBalance)
    } yield {
      def populations = wealths.map { wealthToPopulation }

      def savings =
        s.cities.map(_.wealth * wealthSavingRate)

      val newCities =
        (s.cities zip populations zip wealths zip savings).map(flatten).map {
          case (c, p, w, s) =>
            assert(p >= 0)
            assert(w > 0, s"The city too poor for the model $w, $p")
            copy(c)(population = p, wealth = aboveOne(w), saving = s)
        }

      copy(s)(step = s.step + 1, cities = newCities)
    }
  }

  def wealthToPopulation(wealth: Double): Double

  def wealths(s: STATE, tbs: Seq[Double])(implicit rng: Random): Writer[Seq[LOGGING], Seq[Double]] = {
    val supplies = s.cities.map(c => supply(c.population, c.wealth))
    val demands = s.cities.map(c => demand(c.population))

    val Matched(transactions, unsolds, unsatisfieds) = matchCities(s, supplies, demands)

    val transactedFrom: Map[Int, Seq[Transaction]] =
      transactions.groupBy(_.from).withDefaultValue(Seq.empty)

    val transactedTo: Map[Int, Seq[Transaction]] =
      transactions.groupBy(_.to).withDefaultValue(Seq.empty)

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

    (s.cities zip supplies zip demands zip unsolds zip unsatisfieds zip bonuses zip tbs).map(flatten).map {
      case (city, supply, demand, unsold, unsatified, bonus, tb) =>
        city.wealth + supply - internalShare * demand * 2 + demand - fixedCost + bonus - unsold + unsatified + tb
    }.set(transactions)
  }

  def consumption(population: Double) = adjustConsumption * math.log(population + 1)

  def productivity(wealth: Double) = adjustProductivity * math.log(wealth + 1)

  def demand(population: Double) = consumption(population) * population

  def supply(population: Double, wealth: Double) = productivity(wealth) * population

  def territoryBalance(s: Seq[CITY]): Seq[Double] = {
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

  def distances(implicit rng: Random) = {
    val positions = positionDistribution(rng).toIndexedSeq

    positions.zipWithIndex.map {
      case (c1, i) =>
        positions.zipWithIndex.map { case (c2, _) => distance(c1, c2) }
    }
  }

}
