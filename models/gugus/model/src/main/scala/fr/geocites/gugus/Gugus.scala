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

import scalaz.WriterT

case class Activity (sizeEffectOnDemand: Double, sizeEffectOnSupply: Double, exchangeRate: Double)


trait Gugus <: StepByStep
    with TimeEndingCondition
    with Balances
    with NoLog {

  type LOGGING = Interaction
  type CITY

  def activities: Seq[Activity]

  def economicMultiplier: Double
  def wealthToPopulationExponent: Double

  def cities: Lens[STATE, Seq[CITY]]
  def population: Lens[CITY, Double]
  def wealth: Lens[CITY, Double]
  def network: Lens[STATE, Network]
  def distances: Lens[STATE, DistanceMatrix]

  def nextState(s: STATE)(implicit rng: Random): Log[STATE] = {
    for {
      newWealths <- wealths(s)
    } yield {
      def newPopulations =
        (cities.get(s) zip newWealths).zipWithIndex.map {
          case ((city, newWealth), i) =>
            check(newWealth >= 0, s"City $i error in wealth before conversion toPop $newWealth")
            val deltaPopulation = (wealthToPopulation(newWealth) - wealthToPopulation(wealth.get(city))) / economicMultiplier
            val newPopulation = population.get(city) + deltaPopulation
            check(newPopulation >= 0, s"Error in population $newWealth $newPopulation")
            newPopulation
        }

      def newCities =
        (cities.get(s) zip newPopulations zip newWealths).map(flatten).map {
          case (city, newPopulation, newWealth) =>
            check(newPopulation >= 0, s"The population of $city is negative $newPopulation, $newWealth")
            population.set(newPopulation)(wealth.set(newWealth)(city))
        }

      def updatedState = urbanTransition(cities.set(newCities)(s))
      step.modify(_ + 1)(updatedState)
    }
  }

  def wealths(s: STATE)(implicit rng: Random): Log[Seq[Double]]  = {
    def deltaWealth(activity: Activity): Log[Seq[Double]] = {
      val suppliesOfCities = supplies(cities.get(s), activity)
      val demandsOfCities = demands(cities.get(s), activity)
      for {
        bs <- balances(s, suppliesOfCities, demandsOfCities)
      } yield {
        (suppliesOfCities zip demandsOfCities zip bs).map {
            case ((supply, demand), balance) => supply - demand + balance
          }
      }
    }

    def originalWealth = cities.get(s) map wealth.get

    for {
      dw <- activities.map(deltaWealth).combine
    } yield {
      def newWealths: Seq[Double] =
        (originalWealth zip dw.transpose.map(_.sum)) map {
          case (currentWealth, delta) =>
            val wealth = currentWealth + delta
            if (currentWealth <= 0.0 || delta <= 0.0) 0.0 else wealth
        }

      resourcesEffect(cities.get(s), newWealths)
    }
  }

  def urbanTransition(state: STATE): STATE = state
  def resourcesEffect(cities: Seq[CITY], newWealths: Seq[Double]) = newWealths

  def supplies(cities: Seq[CITY], activity: Activity) = cities.map(c => supply(population.get(c), activity))
  def demands(cities: Seq[CITY], activity: Activity) = cities.map(c => demand(population.get(c), activity))

  def demand(population: Double, activity: Activity) = economicMultiplier * activity.exchangeRate * pow(population, activity.sizeEffectOnDemand)
  def supply(population: Double, activity: Activity) = economicMultiplier * activity.exchangeRate * pow(population, activity.sizeEffectOnSupply)

  def wealthToPopulation(wealth: Double) = {
    check(wealth >= 0, s"Negative wealth $wealth")
    pow(wealth, wealthToPopulationExponent)
  }

}

