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

package fr.geocites.simpoplocal

import fr.geocites.simpuzzle.city.{ Id, Radius, Position }
import scala.collection.immutable.{ TreeSet, TreeMap }
import fr.geocites.simpuzzle.state._
import monocle.Macro._

trait SimpopLocalState <: State {

  type STATE = SimpopLocalState

  /**
   *
   * @param step Simulation step.
   * @param settlements Set of settlement entities.
   * @param currentInnovationId Next id available for innovation creation.
   */
  case class SimpopLocalState(step: Int, settlements: Seq[Settlement], currentInnovationId: Int = 0)

  def step = mkLens[STATE, Int]("step")

  /**
   *
   * Class representing a settlement
   *
   * @param id Id of the settlement in the original data file.
   * @param x Horizontal coordinate.
   * @param y Vertical coordinate.
   * @param population Population of the settlement.
   * @param availableResource Amount of available resource: counted in number of inhabitants that this amount can sustain.
   * @param innovations Set of innovation entities acquired by the city.
   */
  case class Settlement(
    id: Int,
    x: Double,
    y: Double,
    population: Double,
    availableResource: Double,
    innovations: Set[Innovation])

  /**
   * Class representing innovations
   *
   * @param step Date of acquisition
   * @param rootId Id of the original created innovation from which this is copied.
   * @param id Id of the innovation. Equal to id if this innovation if it is a created (root) innovation.
   */
  case class Innovation(step: Int, rootId: Int, id: Int)

}
