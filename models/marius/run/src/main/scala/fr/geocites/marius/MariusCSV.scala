/*
 * Copyright (C) 15/01/14 Romain Reuillon
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

package fr.geocites.marius

import fr.geocites.simpuzzle._
import scalax.io.Resource
import java.io.File
import util.{Success, Failure}
import monocle.syntax._

object BonusFixedCostTest extends BonusFixedCostModel(
  bonusMultiplier = 564.646869914297,
  fixedCost = 0.427446768353976,
  distanceDecay = 0.67639638323395,
  sizeEffectOnSupply = 1,
  sizeEffectOnDemand = 1.0841916528743,
  economicMultiplier = 0.589041240155966,
  populationToWealthExponent = 1.06919766558929,
  wealthToPopulationExponent = 0.410661076332697
)

object ResourceBonusTest extends ResourceBonusModel(
  bonusMultiplier = 58.5435812361961,
  fixedCost = 0.0793245336722257,
  distanceDecay = 0.000291097872621446,
  sizeEffectOnSupply = 1.00140816140032,
  sizeEffectOnDemand = 1.14688384320373,
  economicMultiplier = 0.406724246063534,
  populationToWealthExponent = 1.16091624780169,
  wealthToPopulationExponent = 0.448562632081331,
  oilAndGazEffect = 0.00802829621343371,
  coalEffect = -0.0463216918040995
)

object NationalRedistributionBonusTest extends NationalRedistributionBonusModel(
  bonusMultiplier = 564.646869914297,
  fixedCost = 0.427446768353976,
  distanceDecay = 0.67639638323395,
  sizeEffectOnSupply = 1,
  sizeEffectOnDemand = 1.0841916528743,
  economicMultiplier = 0.589041240155966,
  populationToWealthExponent = 1.06919766558929,
  wealthToPopulationExponent = 0.410661076332697,
  territorialTaxes = 0.8,
  capitalShareOfTaxes = 0.5
)


object RegionalRedistributionBonusTest extends RegionalRedistributionBonusModel(
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

object SingleRedistributionResourceBonusTest extends SingleRedistributionResourceBonusModel(
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


object MariusCSV extends App {

  lazy val models = List(BonusFixedCostTest, ResourceBonusTest, NationalRedistributionBonusTest, RegionalRedistributionBonusTest, DoubleRedistributionBonusTest, DoubleRedistributionResourceBonusTest, SingleRedistributionResourceBonusTest)

  println(Console.YELLOW + "Choose you model: ")
  models.map(_.getClass.getName).zipWithIndex.foreach{ case(c, i) => println( Console.GREEN + s"$i -> ${Console.GREEN} $c") }
  val i = io.StdIn.readInt()
  print(Console.RESET)

  run(models(i))

  def run(m: Marius) = {
    implicit val rng = fr.geocites.simpuzzle.random(42)
    import m._

    val path = "/tmp/mariusmodel_log.csv"

    new File(path).delete

    val out = Resource.fromFile(path)

    out.append("step, arokato, population, wealth \n")

    for {
      (log, cptr) <- m.logs zipWithIndex
    } log.value match {
      case Success(s) =>
        val cs = s |-> cities get
        val transacted = log.written

        for {
          (city, arokato, i) <- (cs zip MariusFile.arokatos).zipWithIndex.map(flatten)
        } {
          def line = Seq(cptr, arokato, city |-> population get, city |-> wealth get)
          out.append(line.mkString("", ",", "\n"))
        }
        val totalWealth = cs.map(_ |-> wealth get).sum
        val totalPop = cs.map(_ |-> population get).sum

        println("Etat ", cptr, " Wealth totale", totalWealth, " pop totale", totalPop)
      case Failure(e) => println(s"Invadid State $e")
    }
  }

}
