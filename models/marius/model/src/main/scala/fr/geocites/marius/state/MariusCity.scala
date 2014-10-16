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

package fr.geocites.marius.state

import monocle._
import scala.util.Random
import fr.geocites.simpuzzle.flatten
import fr.geocites.marius._
import MariusFile._

object MariusCity {
  case class City(
    population: Double,
    wealth: Double,
    region: String,
    nation: String,
    regionalCapital: Boolean,
    nationalCapital: Boolean,
    oilOrGaz: Boolean,
    coal: Boolean)
}

trait MariusCity <: Marius {

  type CITY = MariusCity.City
  def population = Lenser[CITY](_.population)
  def wealth = Lenser[CITY](_.wealth)
  def regionalCapital = Lenser[CITY](_.regionalCapital)
  def region = Lenser[CITY](_.region)
  def nation = Lenser[CITY](_.nation)
  def nationalCapital = Lenser[CITY](_.nationalCapital)
  def oilOrGaz = Lenser[CITY](_.oilOrGaz)
  def coal = Lenser[CITY](_.coal)

  def initialCities(implicit rng: Random) = {
    val pop = initialPopulations.toSeq
    val initialWealths = rescaleWealth(initialPopulations.map(initialWealth), pop)

    (for {
      (_population, _region, _nation, _regionalCapital, _nationalCapital, _oilOrGaz, _coal, _initialWealth) <- pop.toIterator zip regions zip MariusFile.nations zip regionCapitals zip nationalCapitals zip oilOrGazDistribution.toIterator zip coalDistribution.toIterator zip initialWealths.toIterator map (flatten)
    } yield {
      MariusCity.City(
        population = _population,
        region = _region,
        nation = _nation,
        regionalCapital = _regionalCapital,
        nationalCapital = _nationalCapital,
        wealth = _initialWealth,
        oilOrGaz = _oilOrGaz,
        coal = _coal
      )
    }).take(nbCities).toVector
  }
}
