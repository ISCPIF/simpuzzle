/*
 * Copyright (C) 2014 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.geocites.marius

import fr.geocites.gugus.balance.{ InitialWealth, SuperLinearInitialWealth }
import fr.geocites.gugus.urbanisation.UrbanisationFunction
import fr.geocites.simpuzzle._
import fr.geocites.gugus._
import fr.geocites.gugus.structure._
import monocle._

import scala.util.Random

case class State(
  step: Int,
  cities: Seq[City],
  regions: Seq[Region],
  network: Network,
  distanceMatrix: DistanceMatrix)

case class City(
  population: Double,
  wealth: Double,
  region: String,
  nation: String,
  regionalCapital: Boolean,
  nationalCapital: Boolean,
  oilOrGaz: Boolean,
  coal: Boolean)

case class Region(id: String, urbanisationStep: Double)

trait Marius <: Gugus with SuperLinearInitialWealth with MariusFile with UrbanisationFunction {

  type STATE = State
  type CITY = City
  type REGION = Region

  def census: Int

  def step = Lenser[STATE](_.step)

  def cities = Lenser[STATE](_.cities)
  def regions = Lenser[STATE](_.regions)
  def network = Lenser[STATE](_.network)
  def distances = Lenser[STATE](_.distanceMatrix)
  def population = Lenser[CITY](_.population)
  def wealth = Lenser[CITY](_.wealth)
  def regionalCapital = Lenser[CITY](_.regionalCapital)
  def region = Lenser[CITY](_.region)
  def nation = Lenser[CITY](_.nation)
  def nationalCapital = Lenser[CITY](_.nationalCapital)
  def oilOrGaz = Lenser[CITY](_.oilOrGaz)
  def coal = Lenser[CITY](_.coal)

  type TERRITORY = REGION
  def territory: SimpleLens[CITY, String] = region
  def territories: SimpleLens[STATE, Seq[TERRITORY]] = regions
  def urbanisationStep: SimpleLens[TERRITORY, Double] = Lenser[TERRITORY](_.urbanisationStep)
  def territoryId: SimpleLens[TERRITORY, String] = Lenser[TERRITORY](_.id)

  // This parameter has been empirically estimated
  def urbanisationSpeed: Double = 0.017006722508654093

  def initialState(implicit rng: Random) = {
    val cities = initialCities
    State(0, initialCities.toVector, initialRegions, Network.full(cities.size), distanceMatrix)
  }

  def initialRegions =
    (regionIDs.toSeq zip initialUrbanisationRates) map { case (id, rate) => Region(id, inverseUrbanisationFunction(rate)) }

  def initialCities(implicit rng: Random) = {

    val pop = initialPopulations.toSeq
    val initialWealths = InitialWealth.rescaleWealth(pop.map(initialWealth), pop)

    val cities =
      for {
        (_population,
          _region,
          _nation,
          _regionalCapital,
          _nationalCapital,
          _oilOrGaz,
          _coal,
          _initialWealth) <- pop.toIterator zip cityRegions zip cityNations zip regionalCapitals zip nationalCapitals zip oilOrGazDistribution.toIterator zip coalDistribution.toIterator zip initialWealths.toIterator map (flatten)
      } yield {
        City(
          population = _population,
          region = _region,
          nation = _nation,
          regionalCapital = _regionalCapital,
          nationalCapital = _nationalCapital,
          wealth = _initialWealth,
          oilOrGaz = _oilOrGaz,
          coal = _coal
        )
      }
    cities.take(nbCities).toVector
  }

}

