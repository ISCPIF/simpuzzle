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
import fr.geocite.gis.distance.GeodeticDistance
import scalaz._
import scala.util.Random

trait Marius <: StepByStep
    with TimeEndingCondition
    with MariusLogging
    with Matching
    with MariusFile
    with PositionDistribution {

  type CITY

  def cities: Lens[STATE, Seq[CITY]]
  def population: Lens[CITY, Double]

  // calibré sur les villes brésiliennes, pour que les grandes villes consomment et produisent plus que les plus petites
  // selon la formule : Revenu/hab(=productivity) = sizeEffectOnEco * ln (Pop) + gamma
  def sizeEffectOnConsumption: Double

  def sizeEffectOnProductivity: Double

  def gamma: Double

  def territorialTaxes: Double

  def capitalShareOfTaxes: Double

  def wealth: Lens[CITY, Double]
  def region: Lens[CITY, String]
  def capital: Lens[CITY, Boolean]

  def distanceMatrix: Lens[STATE, DistanceMatrix]

  def nextState(s: STATE)(implicit rng: Random) = {

    //def aboveOne(v: Double) = if (v <= 1) 1.0 else v
    val tBalance = territoryBalance(cities.get(s))
    //val nBalance = nationalBalance(cities.get(s))

    for {
      wealths <- wealths(s, tBalance)
    } yield {
      def populations = wealths.map { wealthToPopulation }

      val newCities =
        (cities.get(s) zip populations zip wealths).map(flatten).map {
          case (c, p, w) =>
            assert(p >= 0, s"The population is negative $p, $w")
            assert(w >= 0, s"The city too poor for the model $w, $p")
            wealth.set(population.set(c, p), w)
        }

      cities.set(step.mod(_ + 1, s), newCities)
    }

  }

  def wealthToPopulation(wealth: Double): Double

  def wealths(s: STATE, tbs: Seq[Double])(implicit rng: Random) = {
    val supplies = cities.get(s).map(c => supply(population.get(c)))
    val demands = cities.get(s).map(c => demand(population.get(c)))

    val Matched(transactions, unsolds, unsatisfieds) = matchCities(s, supplies, demands)

    val transactedFrom: Map[Int, Seq[Transaction]] =
      transactions.groupBy(_.from).withDefaultValue(Seq.empty)

    val transactedTo: Map[Int, Seq[Transaction]] =
      transactions.groupBy(_.to).withDefaultValue(Seq.empty)

    def bonuses = {
      val allTransactionsDist =
        for {
          cid <- 0 until cities.get(s).size
          tfrom = transactedFrom(cid)
          tto = transactedTo(cid)
        } yield tfrom ++ tto

      def nbDist = allTransactionsDist.map(_.size.toDouble).centered.reduced
      def nbTotal = allTransactionsDist.map(_.map(_.transacted).sum).centered.reduced
      (nbDist zip nbTotal).map { case (x, y) => x + y }
    }

    log(
      (cities.get(s) zip supplies zip demands zip unsolds zip unsatisfieds zip tbs //zip nbs
      zipWithIndex).map(flatten).map {
        case (city, supply, demand, unsold, unsatisfied, tb, i) =>
          if (i == 0) {
            val calc = wealth.get(city) + supply - demand + unsatisfied
            //println("Wealth ", wealth.get(city), "supply " , supply, "demande ", demand, "unsatisfied ", unsatisfied , "pop" , wealthToPopulation(wealth.get(city) ))
            //	println("Wealth ", wealth.get(city), "supply", supply, "demand", demand, "unsat",unsatisfied ,"calc" , calc)
          }
          wealth.get(city) +
            supply -
            demand -
            unsold +
            unsatisfied +
            tb
        //+ nb

      }.map {

        w => if (w >= 0) w else 0

      },
      transactions)
  }

  def consumption(population: Double) = sizeEffectOnConsumption * math.log(population + 1) + gamma

  def productivity(population: Double) = sizeEffectOnProductivity * math.log(population + 1) + gamma

  def demand(population: Double) = consumption(population) * population

  def supply(population: Double) = productivity(population) * population

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
