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

package fr.geocites.indus

import fr.geocites.gugus.MonoActivity
import fr.geocites.gugus.balance._
import fr.geocites.gugus.transaction._

object Models {

  trait From1961To2011 {
    /** Simulate from 1961 to 2011 */
    def steps = 50
  }

  case class SimpleModel(
    economicMultiplier: Double,
    sizeEffectOnSupply: Double,
    sizeEffectOnDemand: Double,
    distanceDecay: Double,
    wealthToPopulationExponent: Double,
    populationToWealthExponent: Double) extends Indus with From1961To2011 with ProportionalTransaction with MonoActivity

  case class BonusFixedCostModel(
    economicMultiplier: Double,
    sizeEffectOnSupply: Double,
    sizeEffectOnDemand: Double,
    distanceDecay: Double,
    wealthToPopulationExponent: Double,
    populationToWealthExponent: Double,
    bonusMultiplier: Double,
    fixedCost: Double) extends Indus with Bonus with FixedCostTransaction with From1961To2011 with MonoActivity

}
