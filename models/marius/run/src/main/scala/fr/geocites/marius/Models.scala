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

import fr.geocites.gugus.balance._
import fr.geocites.gugus.transaction._

object Models {
  lazy val all = List(BonusFixedCostTest, ResourceBonusTest, DoubleRedistributionBonusTest, DoubleRedistributionResourceBonusTest)
}

/** Default values for some parameters of the models */
trait From59To89 {
  /** Simulate from 1959 to 1989 */
  def steps = 30
  def census = 0
}

/** Model with fixed costs and bonuses */
class BonusFixedCostModel(
  val economicMultiplier: Double,
  val sizeEffectOnSupply: Double,
  val sizeEffectOnDemand: Double,
  val distanceDecay: Double,
  val wealthToPopulationExponent: Double,
  val populationToWealthExponent: Double,
  val bonusMultiplier: Double,
  val fixedCost: Double) extends Marius with Bonus with FixedCostTransaction with From59To89

object BonusFixedCostTest extends BonusFixedCostModel(
  bonusMultiplier = 197.9488907791,
  fixedCost = 0.2565248068,
  distanceDecay = 0.6722631615,
  sizeEffectOnSupply = 1.001756388,
  sizeEffectOnDemand = 1.0792607803,
  economicMultiplier = 0.3438093442,
  populationToWealthExponent = 1.0866012754,
  wealthToPopulationExponent = 0.3804356044
)

class DoubleRedistributionBonusModel(
  val economicMultiplier: Double,
  val sizeEffectOnSupply: Double,
  val sizeEffectOnDemand: Double,
  val distanceDecay: Double,
  val wealthToPopulationExponent: Double,
  val populationToWealthExponent: Double,
  val bonusMultiplier: Double,
  val fixedCost: Double,
  val territorialTaxes: Double,
  val capitalShareOfTaxes: Double)
    extends Marius with Bonus with FixedCostTransaction with DoubleRedistribution with From59To89

object DoubleRedistributionBonusTest extends DoubleRedistributionBonusModel(
  bonusMultiplier = 564.646869914297,
  fixedCost = 0.427446768353976,
  distanceDecay = 0.67639638323395,
  sizeEffectOnSupply = 1,
  sizeEffectOnDemand = 1.0841916528743,
  economicMultiplier = 0.589041240155966,
  populationToWealthExponent = 1.06919766558929,
  wealthToPopulationExponent = 0.410661076332697,
  territorialTaxes = 0.5,
  capitalShareOfTaxes = 0.0
)

class DoubleRedistributionResourceBonusModel(
  val economicMultiplier: Double,
  val sizeEffectOnSupply: Double,
  val sizeEffectOnDemand: Double,
  val distanceDecay: Double,
  val wealthToPopulationExponent: Double,
  val populationToWealthExponent: Double,
  val bonusMultiplier: Double,
  val fixedCost: Double,
  val territorialTaxes: Double,
  val capitalShareOfTaxes: Double,
  val oilAndGazEffect: Double,
  val coalEffect: Double)
    extends Marius with Bonus with FixedCostTransaction with DoubleRedistribution with SubSurfaceResources with From59To89

object DoubleRedistributionResourceBonusTest extends DoubleRedistributionResourceBonusModel(
  bonusMultiplier = 564.646869914297,
  fixedCost = 0.427446768353976,
  distanceDecay = 0.67639638323395,
  sizeEffectOnSupply = 1,
  sizeEffectOnDemand = 1.0841916528743,
  economicMultiplier = 0.589041240155966,
  populationToWealthExponent = 1.06919766558929,
  wealthToPopulationExponent = 0.410661076332697,
  territorialTaxes = 1.0,
  capitalShareOfTaxes = 0.0,
  oilAndGazEffect = 0.0,
  coalEffect = 0.0
)

class ResourceBonusModel(
  val economicMultiplier: Double,
  val sizeEffectOnSupply: Double,
  val sizeEffectOnDemand: Double,
  val distanceDecay: Double,
  val wealthToPopulationExponent: Double,
  val populationToWealthExponent: Double,
  val bonusMultiplier: Double,
  val fixedCost: Double,
  val oilAndGazEffect: Double,
  val coalEffect: Double) extends Marius with Bonus with FixedCostTransaction with SubSurfaceResources with From59To89

object ResourceBonusTest extends ResourceBonusModel(
  bonusMultiplier = 92.450713956686,
  fixedCost = 0.000378558734773219,
  distanceDecay = 0.000477019616908816,
  sizeEffectOnSupply = 1.61102159300671,
  sizeEffectOnDemand = 1.08953233750212,
  economicMultiplier = 0.00610893407538784,
  populationToWealthExponent = 1.23133919109546,
  wealthToPopulationExponent = 0.555223734975376,
  oilAndGazEffect = -0.00256055211048378,
  coalEffect = -0.022116545015663
)

/** Simple model with only core mechanisms */
class SimpleModel(
  val economicMultiplier: Double,
  val sizeEffectOnSupply: Double,
  val sizeEffectOnDemand: Double,
  val distanceDecay: Double,
  val wealthToPopulationExponent: Double,
  val populationToWealthExponent: Double) extends Marius with From59To89 with ProportionalTransaction

