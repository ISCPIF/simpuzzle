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
import scala.util.Random
import scala.math._
import fr.geocite.simpuzzle.state.{ TimeEndingCondition, StepByStep }
import fr.geocite.marius.balance.{ Balances, Exchange }
import fr.geocite.gis.distance.GeodeticDistance
import fr.geocite.marius.structure.Network
import monocle._
import monocle.syntax._

object Marius extends GeodeticDistance {
  lazy val distanceMatrix: DistanceMatrix = {
    val positions = MariusFile.positionDistribution.toVector

    positions.zipWithIndex.map {
      case (c1, i) =>
        positions.zipWithIndex.map { case (c2, _) => distance(c1, c2) }
    }
  }

}

trait Marius <: StepByStep
    with TimeEndingCondition
    with MariusLogging
    with Balances
    with MariusFile {

  type CITY

  def sizeEffectOnConsumption: Double
  def sizeEffectOnProductivity: Double
  def sizeEffectOnInitialWealth: Double

  def cities: SimpleLens[STATE, Seq[CITY]]
  def population: SimpleLens[CITY, Double]
  def wealth: SimpleLens[CITY, Double]
  def region: SimpleLens[CITY, String]
  def nation: SimpleLens[CITY, String]
  def regionalCapital: SimpleLens[CITY, Boolean]
  def nationalCapital: SimpleLens[CITY, Boolean]
  def network: SimpleLens[STATE, Network]

  lazy val distanceMatrix: DistanceMatrix = Marius.distanceMatrix

  def nextState(s: STATE)(implicit rng: Random) = {
    for {
      newWealths <-  wealths(s)
    } yield {
      def populations =
        (cities.get(s) zip newWealths).zipWithIndex.map {
          case ((city, newWealth), i) =>
            check(newWealth >= 0, s"City $i error in wealth before conversion toPop $newWealth")
            val deltaPopulation = wealthToPopulation(newWealth) - wealthToPopulation(wealth.get(city))
            val newPopulation = population.get(city) + deltaPopulation
            check(newPopulation >= 0, s"Error in population $newWealth $newPopulation")
            newPopulation
        }

      def newCities =
        (cities.get(s) zip populations zip newWealths).map(flatten).map {
          case (c, p, w) =>
            check(p >= 0, s"The population of $c is negative $p, $w")
            wealth.set(population.set(c, p), w)
        }

      (s |-> cities set newCities) |-> step modify (_ + 1)
    }
  }

  def wealths(s: STATE)(implicit rng: Random) = {
    val ss = supplies(cities.get(s))
    val ds = demands(cities.get(s))

    for {
      bs <- balances(s, ss, ds)
    } yield {
      (cities.get(s) zip
        ss zip
        ds zip
        bs zipWithIndex).map(flatten).map {
        case (city, supply, demand, b, i) =>
          val newWealth =
            wealth.get(city) + supply - demand + b
          if (newWealth <= 0.0) 0.0 else newWealth
      }
    }
  }

  def consumption(population: Double) = sizeEffectOnConsumption * math.log(population + 1.0)
  def productivity(population: Double) = sizeEffectOnProductivity * math.log(population + 1.0)

  def supplies(cities: Seq[CITY]) = cities.map(c => supply(population.get(c)))
  def demands(cities: Seq[CITY]) = cities.map(c => demand(population.get(c)))

  def demand(population: Double) = consumption(population) * population

  def supply(population: Double) = {
    val s = productivity(population) * population
    check(s >= 0, s"Supply is not good, $s $population")
    s
  }

  private lazy val a = coeffA(popMin, popMax, wMin, wMax, inversionPoint)
  private lazy val b = coeffB(popMin, popMax, wMin, wMax, inversionPoint)
  private lazy val c = 0

  def popMin: Double
  def popMax: Double
  def inversionPoint: Double
  def wMin: Double
  def wMax: Double

  def denominator(popMin: Double, popMax: Double, inversionPoint: Double): Double = 2 * inversionPoint * popMin - 2 * inversionPoint * popMax - pow(popMin, 2) + pow(popMax, 2)

  def coeffA(popMin: Double, popMax: Double, wMin: Double, wMax: Double, inversionPoint: Double): Double = {
    (popMin - popMax - wMin + wMax) / denominator(popMin, popMax, inversionPoint)
  }

  def coeffB(popMin: Double, popMax: Double, wMin: Double, wMax: Double, inversionPoint: Double): Double = {
    (2 * inversionPoint * wMin - 2 * inversionPoint * wMax - pow(popMin, 2) + pow(popMax, 2)) / denominator(popMin, popMax, inversionPoint)
  }

  def rescaleWealth(wealth: Seq[Double], population: Seq[Double]) = {
    val factor = population.sum / wealth.sum.toDouble
    wealth.map(_ * factor)
  }

  def initialWealth(population: Double)(implicit rng: Random): Double = pow(population, sizeEffectOnInitialWealth)

  def wealthToPopulation(wealth: Double) = {
    check(wealth >= 0, s"Negative wealth $wealth")
    (-b + sqrt(pow(b, 2) - 4 * a * (c - wealth))) / (2 * a)
  }

}

