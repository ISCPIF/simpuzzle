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

import fr.geocite.simpuzzle.city.{ Id, Radius, Position }
import scala.collection.immutable.{ TreeSet, TreeMap }

trait SimpopLocalState extends fr.geocite.simpuzzle.State {

  type STATE = SimpopLocalState

  /**
   *
   * @param step Simulation step.
   * @param settlements Set of settlement entities.
   * @param currentInnovationId Next id available for innovation creation.
   */
  case class SimpopLocalState(step: Int, settlements: Seq[Settlement], currentInnovationId: Int = 0)

  /**
   * Order function of innovation. In each settlement innovations are kept ordered by rootId to speed
   * up the computation of the differences between sets of innovations acquired by 2 settlements.
   *
   * By design, each settlements cannot acquire innovations with the same rootId.
   */
  implicit lazy val innovationOrdering = Ordering.by((_: Innovation).rootId)

  /**
   *
   * Class representing a settlement
   *
   * @param id Id of the settlement in the original data file.
   * @param x Horizontal coordinate.
   * @param y Vertical coordinate.
   * @param population Population of the settlement.
   * @param availableResource Amount of available resource: counted in number of inhabitants that this amount can sustain.
   * @param settlementClass Settlements are distributed in 3 classes depending on their population size: 1 bigger, 2 medium, 3 smaller.
   * @param innovations Set of innovation entities acquired by the city.
   */
  case class Settlement(
      id: Int,
      x: Double,
      y: Double,
      population: Double,
      availableResource: Double,
      settlementClass: Int,
      innovations: Set[Innovation]) extends Position with Radius with Id {

    def rangeRadiusClass1 = 20.0
    def rangeRadiusClass2 = 10.0
    def rangeRadiusClass3 = 5.0

    /**
     *
     * @return the radius of interaction of this settlement.
     */
    def radius =
      settlementClass match {
        case 1 => rangeRadiusClass1
        case 2 => rangeRadiusClass2
        case 3 => rangeRadiusClass3
        case _ => sys.error(s"Invalid settlement class $settlementClass")
      }
  }

  /**
   * Class representing innovations
   *
   * @param date Date of acquisition
   * @param rootId Id of the original created innovation from which this is copied.
   * @param id Id of the innovation. Equal to id if this innovation if it is a created (root) innovation.
   */
  case class Innovation(date: Int, rootId: Int, id: Int)

}
