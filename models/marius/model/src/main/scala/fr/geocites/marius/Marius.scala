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
import monocle.macros._

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

  def step = GenLens[STATE](_.step)

  def cities = GenLens[STATE](_.cities)
  def regions = GenLens[STATE](_.regions)
  def network = GenLens[STATE](_.network)
  def distances = GenLens[STATE](_.distanceMatrix)
  def population = GenLens[CITY](_.population)
  def wealth = GenLens[CITY](_.wealth)
  def regionalCapital = GenLens[CITY](_.regionalCapital)
  def region = GenLens[CITY](_.region)
  def nation = GenLens[CITY](_.nation)
  def nationalCapital = GenLens[CITY](_.nationalCapital)
  def oilOrGaz = GenLens[CITY](_.oilOrGaz)
  def coal = GenLens[CITY](_.coal)

  type TERRITORY = REGION
  def territory: Lens[CITY, String] = region
  def territories: Lens[STATE, Seq[TERRITORY]] = regions
  def urbanisationStep: Lens[TERRITORY, Double] = GenLens[TERRITORY](_.urbanisationStep)
  def territoryId: Lens[TERRITORY, String] = GenLens[TERRITORY](_.id)

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

