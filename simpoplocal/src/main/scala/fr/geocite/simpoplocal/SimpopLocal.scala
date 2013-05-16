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

import fr.geocite.simpuzzle.StepByStep
import java.io.File

object SimpopLocal {
  def apply(
    maxInnovation: Int,
    maxAbundance: Double,
    distanceF: Double,
    pSuccessInteraction: Double,
    pSuccessAdoption: Double,
    innovationFactor: Double) = {

    val (_maxInnovation, _maxAbundance, _distanceF, _pSuccessInteraction, _pSuccessAdoption, _innovationFactor) =
      (maxInnovation, maxAbundance, distanceF, pSuccessInteraction, pSuccessAdoption, innovationFactor)

    new StepByStep with SimpopLocalInitialState with SimpopLocalStep with SimpopLocalTimeInnovationEndingCondition {
      def maxInnovation = _maxInnovation
      def maxAbundance = _maxAbundance
      def distanceF = _distanceF
      def pSuccessAdoption = _pSuccessAdoption
      def pSuccessInteraction = _pSuccessInteraction
      def innovationFactor = _innovationFactor
    }
  }
}
