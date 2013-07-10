/*
 * Copyright (C) 21/05/13 Romain Reuillon
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

import scala.io.Source
import fr.geocite.simpuzzle.distribution._
import fr.geocite.simpuzzle.city.{ Position, Population }
import fr.geocite.gis.distance.GeodeticDistance
import fr.geocite.simpuzzle.distance.GeometricDistance
import fr.geocite.simpuzzle.neighbourhood.GeometricDistanceNeighbourhood

trait MariusFile <: PopulationDistribution
    with HydrocarbonDistribution
    with RegionDistribution
    with CapitalDistribution
    with PositionDistribution {

  def startingCities =
    content.filterNot(l => l(13).isEmpty || l(8).isEmpty)

  def populationDistribution = Distribution(startingCities.map(_(13).toDouble))

  def hydrocarbonDistribution = Distribution(startingCities.map(l => toBoolean(l(8))))

  def positionDistribution =
    Distribution(startingCities.map {
      l => Position(l(5).toDouble, l(4).toDouble)
    })

  def regions = startingCities.map(_(2))

  def capitals = startingCities.map(l => toBoolean(l(7)))

  lazy val content = {
    val input =
      Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream("fr/geocite/marius/marius.csv"))

    try input.getLines.drop(1).map {
      l => l.split(",").toSeq
    }.toList
    finally input.close
  }

  def toBoolean(s: String) =
    s match {
      case "1" => true
      case _ => false
    }

}
