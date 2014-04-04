/*
 * Copyright (C) 03/02/14 Romain Reuillon
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

package fr.geocite.marius

import fr.geocite.simpuzzle._
import distribution._
import fr.geocite.marius.matching.Matching
import fr.geocite.simpuzzle.distribution.PositionDistribution
import scalaz._
import scala.util.Random
import scala.math._
import fr.geocite.simpuzzle.state.{ TimeEndingCondition, StepByStep }

trait Marius <: StepByStep
    with TimeEndingCondition
    with MariusLogging
    with Matching
    with MariusFile
    with PositionDistribution {

  type CITY

  def sizeEffectOnConsumption: Double
  def sizeEffectOnProductivity: Double
  def gamma: Double
  def territorialTaxes: Double
  def capitalShareOfTaxes: Double

  def cities: Lens[STATE, Seq[CITY]]
  def population: Lens[CITY, Double]
  def wealth: Lens[CITY, Double]
  def region: Lens[CITY, String]
  def capital: Lens[CITY, Boolean]
  def distanceMatrix: Lens[STATE, DistanceMatrix]

  def nextState(s: STATE)(implicit rng: Random) =
    try {
      val tBalance = territoryBalance(cities.get(s))
      //val nBalance = nationalBalance(cities.get(s))

      val (ws, transactions) = wealths(s, tBalance)

      def populations =
        ws.zipWithIndex.map {
          case (w, i) =>
            // println("ville",i,"W", w)
            check(w >= 0, s"ville $i error in wealth avant conversion toPop $w")
            val p = wealthToPopulation(w)
            check(p >= 0, s"Error in wealth $w $p")
            p
        }

      def newCities =
        (cities.get(s) zip populations zip ws).map(flatten).map {
          case (c, p, w) =>
            check(p >= 0, s"The population of $c is negative $p, $w")
            wealth.set(population.set(c, p), w)
        }

      log(cities.set(step.mod(_ + 1, s), newCities), transactions)
    } catch {
      case e: AssertionError => InvalidState(e)
    }

  def wealths(s: STATE, tbs: Seq[Double])(implicit rng: Random) = {
    val supplies = cities.get(s).map(c => supply(population.get(c)))
    val demands = cities.get(s).map(c => demand(population.get(c)))

    val Matched(transactions, unsolds, unsatisfieds) = matchCities(s, supplies, demands)

    val transactedFrom: Map[Int, Seq[Transaction]] =
      transactions.groupBy(_.from).withDefaultValue(Seq.empty)

    val transactedTo: Map[Int, Seq[Transaction]] =
      transactions.groupBy(_.to).withDefaultValue(Seq.empty)

    def ws = (cities.get(s) zip supplies zip demands zip unsolds zip unsatisfieds zip tbs //zip nbs
    zipWithIndex).map(flatten).map {
      case (city, supply, demand, unsold, unsatisfied, tb, i) =>
        //assert(supply - demand >= 0, s"suply > demand relationship not good, $supply $demand")
        //assert(unsatisfied - unsold >= 0, s"unsatified > unsold relationship not good, $unsatisfied $unsold")
        val newWealth = wealth.get(city) + supply - demand - unsold + unsatisfied
        if (newWealth <= 0.0) {
          //println("ville",i, "ruinée pour la grandeur de la mère russie")
          0.0
        } else {
          newWealth
        }
    }
    (ws, transactions)
  }

  def consumption(population: Double) = sizeEffectOnConsumption * math.log(population + 1.0) + gamma

  def productivity(population: Double) = sizeEffectOnProductivity * math.log(population + 1.0) + gamma

  def demand(population: Double) = consumption(population) * population

  def supply(population: Double) = {
    val s = productivity(population) * population
    check(s >= 0, s"Supply is not good, $s $population")
    s
  }

  private lazy val a = {
    println("## coeff A", coeffA(popMin, popMax, wMin, wMax, inversionPoint))
    coeffA(popMin, popMax, wMin, wMax, inversionPoint)
  }
  private lazy val b = {
    println("## coeff B", coeffB(popMin, popMax, wMin, wMax, inversionPoint))
    coeffB(popMin, popMax, wMin, wMax, inversionPoint)
  }
  private lazy val c = 0
  def popMin: Double
  def popMax: Double
  def inversionPoint: Double
  def wMin: Double
  def wMax: Double
  def denominator(popMin: Double, popMax: Double, inversionPoint: Double): Double = 2 * inversionPoint * popMin - 2 * inversionPoint * popMax - pow(popMin, 2) + pow(popMax, 2)
  def coeffA(popMin: Double, popMax: Double, wMin: Double, wMax: Double, inversionPoint: Double): Double = {
    assert(inversionPoint < popMax / 2.0)
    (popMin - popMax - wMin + wMax) / denominator(popMin, popMax, inversionPoint)
  }
  def coeffB(popMin: Double, popMax: Double, wMin: Double, wMax: Double, inversionPoint: Double): Double = {
    assert(inversionPoint < popMax / 2.0)
    (2 * inversionPoint * wMin - 2 * inversionPoint * wMax - pow(popMin, 2) + pow(popMax, 2)) / denominator(popMin, popMax, inversionPoint)
  }

  def initialWealth(population: Double)(implicit rng: Random): Double = {
    val wealthInit = a * pow(population, 2) + b * population + c
    check(wealthInit >= 0, s"wealth negative à l'init $wealth")
    wealthInit
  }
  def wealthToPopulation(wealth: Double) = {
    check(wealth >= 0, s"wealth negative $wealth")
    (-b + sqrt(pow(b, 2) - 4 * a * (c - wealth))) / (2 * a)
  }

  /*
  def initialWealth(population: Double)(implicit rng: Random): Double = population

  def wealthToPopulation(wealth: Double) =
    wealth match {
      case x if x >= 0 => x
      case _ => 0.0
    }
         */

  def territoryBalance(s: Seq[CITY]): Seq[Double] = {
    val deltas =
      for {
        (r, cs) <- s.zipWithIndex.groupBy(c => region.get(c._1))
        (cities, indexes) = cs.unzip
      } yield {
        val taxes = cities.map(c => supply(population.get(c)) * territorialTaxes)
        val capitalShare = capitalShareOfTaxes * taxes.sum
        val taxesLeft = taxes.sum - capitalShare
        val regionPopulation = cities.map(c => population.get(c)).sum

        val territorialDeltas = (cities zip taxes).map {
          case (city, cityTaxes) =>
            val populationShare = population.get(city) / regionPopulation

            val delta =
              (if (capital.get(city)) taxesLeft * populationShare + capitalShare
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

// Un autre niveau de péréquation fiscale : les Etats
/*
def nationalBalance(s: Seq[CITY]): Seq[Double] = {
val deltas =
for {
(p, cs) <- s.zipWithIndex.groupBy(c => nations.get(c._1))
(cities, indexes) = cs.unzip
} yield {
val taxes = cities.map(c => supply(population.get(c)) * territorialTaxes)
val capitalShare = capitalShareOfTaxes * taxes.sum
val taxesLeft = taxes.sum - capitalShare
val nationPopulation = cities.map(c => population.get(c)).sum

val nationalDeltas = (cities zip taxes).map {
case (city, cityTaxes) =>
val populationShare = population.get(city) / nationPopulation

val delta =
(if (nationalCapital.get(city)) taxesLeft * populationShare + capitalShare
else taxesLeft * populationShare) - cityTaxes
delta
}
indexes zip nationalDeltas
}
*/
