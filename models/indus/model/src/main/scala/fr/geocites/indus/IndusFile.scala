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

package fr.geocites.indus

import fr.geocites.gis.distance._
import fr.geocites.gugus._
import fr.geocites.simpuzzle.city.Position
import scala.io.Source

object IndusFile {

  /** Read the content of the file */
  def contentCities = {
    val input =
      Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream("fr/geocites/indus/cities.csv"))

    input.getLines.map {
      l => l.split(",").toSeq
    }
  }

  /** Read the header of the csv file */
  def header = contentCities.next

  /** Number of column before the census columns */
  def columnsBeforeDates = header.size - numberOfDates

  /** Read the data part of the csv file */
  def data = contentCities.drop(1).toList

  /** The number of columns of census data */
  def numberOfDates = 5

  /** The dates of the census */
  lazy val dates = header.takeRight(numberOfDates).map(_.toInt)

  /**
   * Column of population at a given date
   *
   * @param date date of observation
   * @return an option containing the population if provided, none otherwise
   */
  def populations(date: Int): Option[Seq[Double]] =
    (dates.indexOf(date) match {
      case -1 => None
      case i => Some(i + columnsBeforeDates)
    }).map {
      c => startingCities.map(_(c).toDouble / 1000)
    }

  /** The cities with known populations for all dates */
  def startingCities =
    data.filter {
      d => d.takeRight(numberOfDates).forall(!_.isEmpty) //&& d(columnsBeforeDates).toDouble >= 10000
    }

  /** Number of cities taken into account */
  def nbCities = startingCities.size

  /** Read the position of the cities */
  def positions =
    (longitudes zip latitudes).map {
      case (long, lat) => Position(long, lat)
    }

  /** Cache of the distance matrix between */
  lazy val distanceMatrix: DistanceMatrix = positions.distanceMatrix

  /** Id of cities */
  def arokatos = startingCities.map(_(0))

  /** Names of the cities */
  def names = startingCities.map(_(1))

  /** Latitudes of the cities in decimal degrees */
  def latitudes = startingCities.map(_(5).toDouble)

  /** Longitudes of the cities in decimal degrees */
  def longitudes = startingCities.map(_(4).toDouble)

  /** Populations of the cities at the first date */
  def initialPopulations = populations(dates.head).get

  def localDistrictNumber = startingCities.map(_(2))

  /** Regions of the cities */
  def districts =
    (states zip localDistrictNumber) map {
      case (s, d) => s"${s}_${d}"
    }

  /** A vector of boolean, true in case a city is a regional capital */
  def districtCapitals =
    startingCities.map(_(7)).map {
      case "1" => true
      case _ => false
    }

  /** States cities belong to */
  def states = startingCities.map(_(3))

  /** A vector of boolean, true in case a city is a national capital */
  def stateCapitals =
    startingCities.map(_(6)).map {
      case "1" => true
      case _ => false
    }

}
