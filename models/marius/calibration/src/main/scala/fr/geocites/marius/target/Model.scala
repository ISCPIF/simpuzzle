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

package fr.geocites.marius.target

import fr.geocite.marius.balance.{FixedCost, Bonus}
import fr.geocite.marius.{ RelatedSizeEffect, Marius }
import fr.geocite.marius.state._
import fr.geocite.marius.matching.ProportionalMatching
import fr.geocite.simpuzzle.logging.NoLogging


trait DefaultValues {
  def popMin: Double = 0
  def wMin: Double = 0
  def territorialTaxes = 0.0
  def capitalShareOfTaxes = 0.0
  def maxStep = 30
}

class CompleteModel(
    val sizeEffectOnProductivity: Double,
    val sizeEffectOnConsumption: Double,
    val distanceDecay: Double,
    val inversionPoint: Double,
    val popMax: Double,
    val wMax: Double,
    val sizeEffectOnInitialWealth: Double) extends Marius with FullNetworkState with ProportionalMatching with NoLogging with DefaultValues

class BonusModel(
    val sizeEffectOnProductivity: Double,
    val sizeEffectOnConsumption: Double,
    val distanceDecay: Double,
    val inversionPoint: Double,
    val popMax: Double,
    val wMax: Double,
    val sizeEffectOnInitialWealth: Double,
    val bonusMultiplier: Double) extends Marius with FullNetworkState with ProportionalMatching with Bonus with NoLogging with DefaultValues


class BonusFixedCostModel(
    val sizeEffectOnProductivity: Double,
    val sizeEffectOnConsumption: Double,
    val distanceDecay: Double,
    val inversionPoint: Double,
    val popMax: Double,
    val wMax: Double,
    val sizeEffectOnInitialWealth: Double,
    val bonusMultiplier: Double,
    val fixedCost: Double) extends Marius with FullNetworkState with ProportionalMatching with Bonus with NoLogging with DefaultValues with FixedCost

