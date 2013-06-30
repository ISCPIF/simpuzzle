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

import fr.geocite.simpuzzle.EndingCondition

trait SimpopLocalTimeInnovationEndingCondition extends EndingCondition with SimpopLocalState {

  def maxDate = 4000
  def maxInnovation: Double

  def ended(state: STATE) =
    state.step >= 4000 || state.currentInnovationId > maxInnovation

}
