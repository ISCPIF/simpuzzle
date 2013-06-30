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
import scala.collection.immutable.TreeSet

trait SimpopLocalInitialState <: InitialState with SimpopLocalState with GeometricDistanceNeighbourhood with EuclideanDistance {

  def initial(implicit rng: Random) = initialState

  lazy val initialState = {

    val input =
      Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream("init-situation.txt"))

    /* Read File to create settlements with the matching attributes */
    val settlements =
      input.getLines.map {
        line =>
          val parsed = line.split(",")
          Settlement(
            id = parsed(0).toInt,
            x = parsed(1).toDouble,
            y = parsed(2).toDouble,
            population = parsed(3).toDouble,
            availableResource = parsed(4).toDouble,
            settlementClass = parsed(5).toInt,
            innovations = TreeSet.empty)
      }.toArray.sortBy(_.id).toIndexedSeq

    /* Create the initial state for the simulation */
    SimpopLocalState(0, settlements)
  }

  /**
   * Settlements interaction network definition.
   */
  lazy val network = {
    import initialState.settlements

    val settlementsClass1 = settlements.filter {
      _.settlementClass == 1
    }
    val settlementsClass2 = settlements.filter {
      _.settlementClass == 2
    }
    val settlementsClass3 = settlements.filter {
      _.settlementClass == 3
    }

    /*
     * Compute the network of settlement possible interactions:
     * for each settlement it computes a list of all the settlements it can interact with and the distance to this settlement
     */
    settlements.map {
      settlement =>
        settlement.settlementClass match {
          case 1 =>
            //All settlements of class 1 are connected to all neighbouring settlements of class 1
            neighbors(settlementsClass1, settlement)
          case 2 =>
            //All settlements of class 2 are connected to all neighbouring settlements (class 3 to 1)
            neighbors(settlementsClass1, settlement) ++
              neighbors(settlementsClass2, settlement) ++
              neighbors(settlementsClass3, settlement)
          case 3 =>
            //All settlements of class 3 are connected to all neighbouring settlements (class 3 to 1)
            neighbors(settlementsClass1, settlement) ++
              neighbors(settlementsClass2, settlement) ++
              neighbors(settlementsClass3, settlement)
        }
    }.toIndexedSeq
  }

}
