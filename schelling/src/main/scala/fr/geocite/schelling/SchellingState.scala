/*
 * Copyright (C) 17/05/13 Romain Reuillon
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

package fr.geocite.schelling

import fr.geocite.simpuzzle._

trait SchellingState <: State {
  def side: Int

  trait Place
  case object Free extends Place
  case object White extends Place
  case object Black extends Place

  case class SchellingState(step: Int, cells: Seq[Seq[Place]]) {
          def positiveMod(i: Int, j: Int) = {
    val m = i % j
    if(m < 0) m + j else m
  }

    def apply(i: Int, j: Int) = cells(positiveMod(i, side))(positiveMod(j, side))

    def cellsIndices =
    cells.zipWithIndex.flatMap{
      case(l, i) => l.zipWithIndex.map{ case(c, j) => ((i, j), c) }
    }

  }

  type STATE = SchellingState
}
