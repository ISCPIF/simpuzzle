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

package fr.geocite.simpoplocal

import fr.geocite.simpuzzle.neighbourhood._
import scala.util.Random
import scala.io.Source
import fr.geocite.simpuzzle.distance._
import fr.geocite.simpuzzle._

trait SimpopLocalInitialState <: InitialState with SimpopLocalState with GeometricDistanceNeighbourhood with EuclideanDistance {

  def rMax: Double

  def initial(implicit rng: Random) = initialState

  lazy val initialState = {

    val input =
      Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream("init-situation.txt"))

    /* Read File to create city, zero line by city
     * 0 > id
     * 1 > x
     * 2 > y
     * 3 > population
     * 4 > own-resource-available
     * 6 > percolation-index
     * 7 > class of city
     */
    val cities =
      input.getLines.map {
        line => line.slice(1, line.size - 1).split("\\s").map(_.toDouble)
      }.map {
        c =>
          new City(
            id = c(0).toInt,
            x = c(1),
            y = c(2),
            population = c(3),
            availableResource = c(4),
            percolationIndex = c(6).toInt,
            cityClass = c(7).toInt,
            innovations = List.empty)
      }.toArray.sortBy((_: City).id).toIndexedSeq

    SimpopLocalState(0, cities)
  }

  lazy val territory = {
    val cities = initialState.cities

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
            neighbors(citiesClass1, city)
          case 2 =>
            //All city of class 2 connected to all other cities (class 3 to 1) in porteRadiusClass2
            neighbors(citiesClass1, city) ++
              neighbors(citiesClass2, city) ++
              neighbors(citiesClass3, city)
          case 3 =>
            //All city of class 3 connected to all other cities (class 3 to 1) in porteRadiusClass3
            neighbors(citiesClass1, city) ++
              neighbors(citiesClass2, city) ++
              neighbors(citiesClass3, city)
        }
    }.toIndexedSeq
  }

}
