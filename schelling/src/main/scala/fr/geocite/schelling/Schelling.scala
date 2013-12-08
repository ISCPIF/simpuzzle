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

import scalaz._

trait Schelling <: SchellingStep {

  type STATE = SchellingState

  case class SchellingState(step: Int, cells: CELLS)

  def step = Lens.lensu[STATE, Int]((s, v) => s.copy(step = v), _.step)
  def cells = Lens.lensu[STATE, CELLS]((s, v) => s.copy(cells = v), _.cells)
}
