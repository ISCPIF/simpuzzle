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

package fr.geocites.schelling

import fr.geocites.simpuzzle._
import scala.util.Random
import fr.geocites.simpuzzle.state.InitialState
import fr.geocites.simpuzzle.logging.NoLog

trait SchellingRandomInitialState <: InitialState
    with Schelling
    with NoLog {

  def freeProportion: Double
  def whiteProportion: Double
  def side: Int

  def initialState(implicit rng: Random) =
    SchellingState(
      0,
      new Cells {
        def cells = Seq.fill(side, side)(randomCell)
      })

  // Randomly draw a cell type given the proportions
  def randomCell(implicit rng: Random): Place =
    if (rng.nextDouble < freeProportion) Free
    else if (rng.nextDouble < whiteProportion) White else Black

}
