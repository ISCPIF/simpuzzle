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

package fr.geocites.marius

import fr.geocites.gis.distance.GeodeticDistance
import fr.geocites.gugus.DistanceMatrix
import fr.geocites.simpuzzle.city.Position

import scala.collection.mutable
import scala.io.Source

object MariusFile {

  lazy val memoization = new mutable.HashMap[Int, MariusFile]

  def apply(census: Int): MariusFile = memoization.synchronized {
    memoization.getOrElseUpdate(
      census, {
        val _census = census
        new MariusFile {
          def census: Int = _census
        }
      }
    )
  }

}

trait MariusFile extends GeodeticDistance {

  def census: Int

  /** Read the content of the file */
  def contentCities= {
    val input =
      Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream("fr/geocites/marius/marius.csv"))

    input.getLines.map {
      l => l.split(",").toSeq
    }
  }

  /** Read the header of the csv file */
  def header = contentCities.next

  /** Read the data part of the csv file */
  def data = contentCities.drop(1).toList

  /** The number of columns of census data */
  def numberOfDates = 6 - census

  /** The dates of the census */
  lazy val dates = header.takeRight(numberOfDates).map(_.toInt)

  /** The cities with known populations for all dates */

  def startingCities =
    data.filter {
      _.takeRight(numberOfDates).forall(!_.isEmpty)
    }

  /** Number of cities taken into account */
  def nbCities = startingCities.size

  /** Read the position of the cities */
  def positions =
    startingCities.map {
      l => Position(l(5).toDouble, l(4).toDouble)
    }

  /** Number of column before the census columns */
  def columnsBeforeDates = header.size - numberOfDates

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
      c => startingCities.map(_(c).toDouble)
    }

  /** Id of cities */
  def arokatos = startingCities.map(_(0))

  /** Names of the cities */
  def names = startingCities.map(_(1))

  /** Latitudes of the cities in decimal degrees */
  def latitudes = startingCities.map(_(4))

  /** Longitudes of the cities in decimal degrees */
  def longitudes = startingCities.map(_(5))

  /** Populations of the cities at the first date */
  def initialPopulations = populations(dates.head).get

  /** Cities with oil and/or gaz */
  def oilOrGazDistribution = startingCities.map(l => toBoolean(l(8)))

  /** Cities with coal */
  def coalDistribution = startingCities.map(l => toBoolean(l(6)))

  /** Regions of the cities */
  def regions = startingCities.map(_(2)).toIterator

  /** A vector of boolean, true in case a city is a regional capital */
  def regionCapitals = startingCities.map(l => toBoolean(l(7))).toIterator

  /** A vector of boolean, true in case a city is a national capital */
  def nationalCapitals = startingCities.map(l => toBoolean(l(9))).toIterator

  /** States cities belong to */
  def nations = startingCities.map(_(3)).toIterator

  /** Cache of the distance matrix between */
  lazy val distanceMatrix: DistanceMatrix = {
    val p = positions.toVector

    p.zipWithIndex.map {
      case (c1, i) =>
        p.zipWithIndex.map { case (c2, _) => distance(c1, c2) }
    }
  }

 

  /** Read the content of the file */
  def contentRegions= {
    val input =
      Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream("fr/geocites/marius/marius-regions.csv"))

    input.getLines.map {
      l => l.split(",").toSeq
    }
  }

  /** Read the header of the csv file */
  def header = contentRegions.next

  /** Read the data part of the csv file */
  def data = contentRegions.drop(1).toList

  /** The number of columns of census data */
  def numberOfDates = 6 - census

  /** The dates of the census */
  lazy val dates = header.takeRight(numberOfDates).map(_.toInt)

  /** The regions with known urbanisation rates for all dates */

  def startingRegions =
    data.filter {
      _.takeRight(numberOfDates).forall(!_.isEmpty)
    }

  /** Number of regions taken into account */
  def nbRegions = startingRegions.size

  /** Read the position of the cities */
  def positionsRegions =
    startingRegions.map {
      l => Position(l(5).toDouble, l(4).toDouble)
    }

  /** Number of column before the census columns */
  def columnsBeforeDates = header.size - numberOfDates

  /**
   * Column of urbanisation rates at a given date
   *
   * @param date date of observation
   * @return an option containing the population if provided, none otherwise
   */
  def urbanisationRates(date: Int): Option[Seq[Double]] =
    (dates.indexOf(date) match {
      case -1 => None
      case i => Some(i + columnsBeforeDates)
    }).map {
      c => startingRegions.map(_(c).toDouble)
    }

  /** Id of regions */
  def IDREG = startingRegions.map(_(0))

  /** Names of the regions */
  def names = startingCities.map(_(1))


  /** Populations of the cities at the first date */
  def initialUrbanisationRates = urbanisationRates(dates.head).get

   /** States cities belong to */
  def nations = startingRegions.map(_(2)).toIterator


  }

  /** A converter function from string to boolean */
  private def toBoolean(s: String) =
    s match {
      case "1" => true
      case _ => false
    }
}