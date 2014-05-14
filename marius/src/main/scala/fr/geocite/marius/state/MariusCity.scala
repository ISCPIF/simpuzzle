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

import scalaz.Lens
import scala.util.Random
import fr.geocite.simpuzzle.flatten
import fr.geocite.simpuzzle.distribution._
import fr.geocite.marius._

object MariusCity {
  case class City(population: Double, wealth: Double, region: String, nation: String, regionalCapital: Boolean, nationalCapital: Boolean)
}

trait MariusCity <: MariusFile with Marius {

  type CITY = MariusCity.City
  def population = Lens.lensu[CITY, Double]((c, v) => c.copy(population = v), _.population)
  def wealth = Lens.lensu[CITY, Double]((c, v) => c.copy(wealth = v), _.wealth)
  def regionalCapital = Lens.lensu[CITY, Boolean]((c, v) => c.copy(regionalCapital = v), _.regionalCapital)
  def region = Lens.lensu[CITY, String]((c, v) => c.copy(region = v), _.region)
  def nation = Lens.lensu[CITY, String]((c, v) => c.copy(nation = v), _.nation)
  def nationalCapital = Lens.lensu[CITY, Boolean]((c, v) => c.copy(nationalCapital = v), _.nationalCapital)

  def initialCities(implicit rng: Random) =
    for {
      (_population, _region, _nation, _regionalCapital, _nationalCapital) <- populations zip regions zip nations zip regionCapitals zip nationalCapitals map (flatten)
    } yield {
      MariusCity.City(
        population = _population,
        region = _region,
        nation = _nation,
        regionalCapital = _regionalCapital,
        nationalCapital = _nationalCapital,
        wealth = initialWealth(_population)
      )
    }
}
