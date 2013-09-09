/*
 * Copyright (C) 09/09/13 Romain Reuillon
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

package fr.geocite.sugarscape

import fr.geocite.simpuzzle._
import fr.geocite.simpuzzle.matrix.Torus2D

trait SugarScapeState <: State {

  def maxSugarCells: Seq[Seq[Int]]

  case class SugarScapeState(step: Int, cells: Seq[Seq[Int]]) extends Torus2D {
    type CELL = Int
  }
}
