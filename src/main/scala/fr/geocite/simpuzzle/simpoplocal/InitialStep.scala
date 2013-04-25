/*
 * Copyright (C) 25/04/13 Romain Reuillon
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

package fr.geocite.simpuzzle.simpoplocal

import java.io.File
import io.Source
import State._
import math._

trait InitialStep extends fr.geocite.simpuzzle.InitialState with State with EuclideanNeighborhood {

  def cityFile: File

  def maxAbundance: Double

  def meanPopulation: Double = 80.0
  def rangeRadiusClass1 = 20.0
  def rangeRadiusClass2 = 10.0
  def rangeRadiusClass3 = 5.0

  lazy val initial = {

    /* Read File to create city, one line by city
     * 0 > id
     * 1 > x
     * 2 > y
     * 3 > population
     * 4 > own-resource-available
     * 6 > percolation-index
     * 7 > class of city
     */
    val cities =
      Source.fromFile(cityFile).getLines.map {
        line => line.slice(1, line.size - 1).split("\\s").map(_.toDouble)
      }.map {
        c =>
          new City(
            id = c(0).toInt,
            x = c(1),
            y = c(2),
            population = c(3),
            availableResource = c(4),
            resourceMax = meanPopulation * maxAbundance,
            percolationIndex = c(6).toInt,
            cityClass = c(7).toInt,
            tradePlace = TradePlace())
      }.toArray.sortBy((_: City).id).toIndexedSeq

    State.SimpopLocalState(0, cities)
  }

  lazy val territory = {
    val cities = initial.cities

    // Store Cities into IndexedSeq[City]
    // create link 1, radius limit = true
    val citiesClass1 = cities.filter {
      _.cityClass == 1
    }
    val citiesClass2 = cities.filter {
      _.cityClass == 2
    }
    val citiesClass3 = cities.filter {
      _.cityClass == 3
    }

    // return city and theirs cities neighbor's  and distance
    // It's a precomputed network which contain also precomputed distance
    cities.map {
      city =>
        city.cityClass match {
          case 1 =>
            //All city of class 1 connected to all city of class 1
            neighbors(citiesClass1, city, rangeRadiusClass1)
          case 2 =>
            //All city of class 2 connected to all other cities (class 3 to 1) in porteRadiusClass2
            neighbors(citiesClass1, city, rangeRadiusClass2) ++
              neighbors(citiesClass2, city, rangeRadiusClass2) ++
              neighbors(citiesClass3, city, rangeRadiusClass2)
          case 3 =>
            //All city of class 3 connected to all other cities (class 3 to 1) in porteRadiusClass3
            neighbors(citiesClass1, city, rangeRadiusClass3) ++
              neighbors(citiesClass2, city, rangeRadiusClass3) ++
              neighbors(citiesClass3, city, rangeRadiusClass3)
        }
    }.toIndexedSeq
  }

}
