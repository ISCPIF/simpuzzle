/*
 * Copyright (C) 2014 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.geocites.gugus.transaction

import fr.geocites.gugus.Gugus
import fr.geocites.gugus.structure._
import monocle.syntax._

object PotentialMatrix {
  case class InteractionPotentialException(message: String, matrix: Matrix) extends AssertionError(message)
}

trait PotentialMatrix <: InteractionPotential { self: Gugus =>

  def interactionPotentialMatrix(state: STATE, supplies: Seq[Double], demands: Seq[Double]) = {
    val iM1 = supplies.toArray
    val iM2 = demands.toArray
    (state |-> network get).mapNodes {
      (i, j) =>
        interactionPotential(iM1(i), iM2(j), (state |-> distances get)(i)(j))
    }
  }

}
