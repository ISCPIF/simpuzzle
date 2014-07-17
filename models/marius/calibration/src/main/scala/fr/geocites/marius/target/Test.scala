/*
 * Copyright (C) 10/12/13 Romain Reuillon
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

package fr.geocites.marius.target

import fr.geocite.marius.Marius
import fr.geocite.marius.balance.{FixedCost, Bonus}
import fr.geocite.marius.matching.ProportionalMatching
import fr.geocite.marius.state.FullNetworkState
import fr.geocite.simpuzzle.logging.NoLogging

import util.Random

object Test extends App {
  //sizeEffectOnConsumption,sizeEffectOnProductivity,gamma,inversionPoint
  //127.1156496908556,38.621747226024965,63.49223984156123,11173.859183000295

  //islandGeneration	sizeEffectOnProductivity	sizeEffectOnConsumption	popMax	wMax	distanceDecay	inversionPoint	sizeEffectOnInitialWealths	distribution
  //8637	0.7582331422	0.1203719503	27549.1333059367	200000	0	967.3001475163	1.1690734226	4.6699534535


 // islandGeneration	sizeEffectOnProductivity	sizeEffectOnConsumption	popMax	wMax	distanceDecay	inversionPoint	sizeEffectOnInitialWealths	distribution
 // 284	0.3814616848	1	26907.7437033396	12084.2530594478	6.9232385874	1000	0.9575024563	0.7860311207



  implicit val rng = new Random

  val m = new Marius with FullNetworkState with ProportionalMatching with NoLogging with Bonus with FixedCost {
    def wealthToPopulationExponent: Double = 1.717899979
    def territorialTaxes = 0.0
    def capitalShareOfTaxes = 0.0
    def distanceDecay =  2.0220171414239
    def sizeEffectOnSupply = 668279.1977789
    def sizeEffectOnDemand =   0.00132415047686369
    def sizeEffectOnInitialWealth: Double =          1.44083648815752
    def fixedCost: Double = 0.0

    def bonusMultiplier : Double =         107.219936151126
    def maxStep = 30
  }

  /*val m = new CompleteModel(
    sizeEffectOnProductivity = 0.3814616848,
    sizeEffectOnConsumption = 1,
    distanceDecay = 6.9232385874,
    inversionPoint = 1000,
    popMax = 26907.743703339,
    wMax = 12084.2530594478,
    sizeEffectOnInitialWealth = 0.9575024563)*/

    println(Calibration.fitness(m).deep)

  //0,0030635465	1,1006449254	176,8439810029

}
