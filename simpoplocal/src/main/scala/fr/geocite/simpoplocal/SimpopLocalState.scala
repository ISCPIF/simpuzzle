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

trait SimpopLocalState extends fr.geocite.simpuzzle.State {

  type STATE = SimpopLocalState

  case class SimpopLocalState(date: Int, cities: Seq[City], currentInnovationId: Int = 0) {
    def step = date
  }

  implicit lazy val innovationOrdering = Ordering.by((_: Innovation).rootId)

  case class City(
      id: Int,
      x: Double,
      y: Double,
      population: Double,
      availableResource: Double,
      percolationIndex: Int,
      cityClass: Int,
      innovations: List[Innovation]) extends Position with Radius with Id {

    def rangeRadiusClass1 = 20.0
    def rangeRadiusClass2 = 10.0
    def rangeRadiusClass3 = 5.0

    def radius =
      cityClass match {
        case 1 => rangeRadiusClass1
        case 2 => rangeRadiusClass2
        case 3 => rangeRadiusClass3
        case _ => sys.error(s"Invalid city class $cityClass")
      }

    lazy val sortedInnovations = innovations.sorted
  }

  class Innovation(
      val city: Int,
      val date: Int,
      val rootId: Int,
      val id: Int) {

    override def toString = "Id=" + id + ", RootId=" + rootId
  }

}
