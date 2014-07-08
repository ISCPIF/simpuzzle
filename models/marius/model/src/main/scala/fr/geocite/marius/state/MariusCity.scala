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

package fr.geocite.marius.state

import scala.util.Random
import fr.geocite.simpuzzle.flatten
import fr.geocite.simpuzzle.distribution._
import fr.geocite.marius._
import monocle.Macro._

object MariusCity {
  case class City(population: Double, wealth: Double, region: String, nation: String, regionalCapital: Boolean, nationalCapital: Boolean)
}

trait MariusCity <: MariusFile with Marius {

  type CITY = MariusCity.City
  def population = mkLens[CITY, Double]("population")
  def wealth = mkLens[CITY, Double]("wealth")
  def regionalCapital = mkLens[CITY, Boolean]("regionalCapital")
  def region = mkLens[CITY, String]("region")
  def nation = mkLens[CITY, String]("nation")
  def nationalCapital = mkLens[CITY, Boolean]("nationalCapital")

  def initialCities(implicit rng: Random) = {
    val pop = initialPopulations.toSeq
    val initialWealths = rescaleWealth(initialPopulations.map(initialWealth), pop)

    (for {
      (_population, _region, _nation, _regionalCapital, _nationalCapital, _initialWealth) <- pop.toIterator zip regions zip nations zip regionCapitals zip nationalCapitals zip initialWealths.toIterator map (flatten)
    } yield {
      MariusCity.City(
        population = _population,
        region = _region,
        nation = _nation,
        regionalCapital = _regionalCapital,
        nationalCapital = _nationalCapital,
        wealth = _initialWealth
      )
    }).take(nbCities).toVector
  }
}
