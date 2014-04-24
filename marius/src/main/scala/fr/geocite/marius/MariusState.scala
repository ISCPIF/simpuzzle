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

package fr.geocite.marius

import scala.util.Random
import scalaz._
import fr.geocite.simpuzzle.distribution._
import fr.geocite.gis.distance.GeodeticDistance
import fr.geocite.simpuzzle._
import scala.reflect.ClassTag

object MariusState {
  case class City(population: Double, wealth: Double, region: String, nation: String, regionalCapital: Boolean, nationalCapital: Boolean)
  case class State(step: Int, cities: Seq[City], distanceMatrix: DistanceMatrix)

}

trait MariusState <: PopulationDistribution
    with RegionDistribution
    with PositionDistribution
    with GeodeticDistance
    with CapitalDistribution
    with MariusFile
    with MariusLogging
    with Marius {

  type CITY = MariusState.City
  type STATE = MariusState.State

  def population = Lens.lensu[CITY, Double]((c, v) => c.copy(population = v), _.population)
  def wealth = Lens.lensu[CITY, Double]((c, v) => c.copy(wealth = v), _.wealth)
  def regionalCapital = Lens.lensu[CITY, Boolean]((c, v) => c.copy(regionalCapital = v), _.regionalCapital)
  def region = Lens.lensu[CITY, String]((c, v) => c.copy(region = v), _.region)
  def nation = Lens.lensu[CITY, String]((c, v) => c.copy(nation = v), _.nation)
  def nationalCapital = Lens.lensu[CITY, Boolean]((c, v) => c.copy(nationalCapital = v), _.nationalCapital)

  def step = Lens.lensu[STATE, Int]((s, v) => s.copy(step = v), _.step)
  def cities = Lens.lensu[STATE, Seq[CITY]]((s, v) => s.copy(cities = v.toVector), _.cities)
  def distanceMatrix = Lens.lensu[STATE, DistanceMatrix]((s, v) => s.copy(distanceMatrix = v), _.distanceMatrix)

  def nbCities: Int

  def initialState(implicit rng: Random) = MariusState.State(0, initialCities.take(nbCities).toVector, distances)

  def initialCities(implicit rng: Random) =
    for {
      (_population, _region, _nation, _regionalCapital, _nationalCapital) <- populations zip regions zip nations zip regionCapitals zip nationalCapitals map (flatten)
    } yield {
      MariusState.City(
        population = _population,
        region = _region,
        nation = _nation,
        regionalCapital = _regionalCapital,
        nationalCapital = _nationalCapital,
        wealth = initialWealth(_population)
      )
    }

  def distances(implicit rng: Random) = {
    val positions = positionDistribution(rng).toVector

    positions.zipWithIndex.map {
      case (c1, i) =>
        positions.zipWithIndex.map { case (c2, _) => distance(c1, c2) }
    }
  }

}
