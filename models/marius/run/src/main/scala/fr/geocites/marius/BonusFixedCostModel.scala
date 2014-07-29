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

package fr.geocites.marius

import fr.geocites.marius.balance._
import fr.geocites.marius.state.FullNetworkState
import fr.geocites.marius.transaction._


/** Model with fixed costs and bonuses */
class BonusFixedCostModel(
  val economicMultiplier: Double,
  val sizeEffectOnSupply: Double,
  val sizeEffectOnDemand: Double,
  val distanceDecay: Double,
  val wealthToPopulationExponent: Double,
  val populationToWealthExponent: Double,
  val bonusMultiplier: Double,
  val fixedCost: Double) extends Marius with Bonus with FixedCostTransaction with DefaultValues with FullNetworkState
