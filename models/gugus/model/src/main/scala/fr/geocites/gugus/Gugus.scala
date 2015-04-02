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

package fr.geocites.gugus

import fr.geocites.gugus.balance._
import fr.geocites.gugus.structure._
import fr.geocites.simpuzzle._
import fr.geocites.simpuzzle.logging.NoLog
import fr.geocites.simpuzzle.state.{ StepByStep, TimeEndingCondition }

import scala.math._
import scala.util.Random
import monocle._
import monocle.syntax._

trait Gugus <: StepByStep
    with TimeEndingCondition
    with Balances
    with NoLog {

  type LOGGING = Interaction

  type CITY

  def economicMultiplier: Double
  def sizeEffectOnDemand: Double
  def sizeEffectOnSupply: Double
  def wealthToPopulationExponent: Double

  def cities: SimpleLens[STATE, Seq[CITY]]
  def population: SimpleLens[CITY, Double]
  def wealth: SimpleLens[CITY, Double]
  def network: SimpleLens[STATE, Network]
  def distances: SimpleLens[STATE, DistanceMatrix]

  def nextState(s: STATE)(implicit rng: Random) = {
    for {
      newWealths <- wealths(s)
    } yield {
      def newPopulations =
        ((s |-> cities get) zip newWealths).zipWithIndex.map {
          case ((city, newWealth), i) =>
            check(newWealth >= 0, s"City $i error in wealth before conversion toPop $newWealth")
            val deltaPopulation = (wealthToPopulation(newWealth) - wealthToPopulation(city |-> wealth get)) / economicMultiplier
            val newPopulation = (city |-> population get) + deltaPopulation
            check(newPopulation >= 0, s"Error in population $newWealth $newPopulation")
            newPopulation
        }

      def newCities =
        ((s |-> cities get) zip newPopulations zip newWealths).map(flatten).map {
          case (city, newPopulation, newWealth) =>
            check(newPopulation >= 0, s"The population of $city is negative $newPopulation, $newWealth")
            (city |-> wealth set newWealth) |-> population set newPopulation
        }

      def updatedState = urbanTransition(s |-> cities set newCities)
      updatedState |-> step modify (_ + 1)
    }
  }

  def wealths(s: STATE)(implicit rng: Random) = {
    val suppliesOfCities = supplies(cities.get(s))
    val demandsOfCities = demands(cities.get(s))

    for {
      balances <- balances(s, suppliesOfCities, demandsOfCities)
    } yield {
      val newWealths =
        (cities.get(s) zip
          suppliesOfCities zip
          demandsOfCities zip
          balances zipWithIndex).map(flatten).map {
          case (city, supply, demand, balance, i) =>
            val currentWealth = city |-> wealth get
            val newWealth = currentWealth + supply - demand + balance
            if (currentWealth <= 0.0 || newWealth <= 0.0) 0.0 else newWealth
        }
      resourcesEffect(s |-> cities get, newWealths)
    }
  }

  def urbanTransition(state: STATE): STATE = state
  def resourcesEffect(cities: Seq[CITY], newWealths: Seq[Double]) = newWealths
  def supplies(cities: Seq[CITY]) = cities.map(c => supply(c |-> population get))
  def demands(cities: Seq[CITY]) = cities.map(c => demand(c |-> population get))

  def demand(population: Double) = economicMultiplier * pow(population, sizeEffectOnDemand)
  def supply(population: Double) = economicMultiplier * pow(population, sizeEffectOnSupply)

  def wealthToPopulation(wealth: Double) = {
    check(wealth >= 0, s"Negative wealth $wealth")
    pow(wealth, wealthToPopulationExponent)
  }

}

