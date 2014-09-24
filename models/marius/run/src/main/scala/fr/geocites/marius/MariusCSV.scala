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
  bonusMultiplier = 197.9488907791,
  fixedCost = 0.2565248068,
  distanceDecay = 0.6722631615,
  sizeEffectOnSupply = 1.001756388,
  sizeEffectOnDemand = 1.0792607803,
  economicMultiplier = 0.3438093442,
  populationToWealthExponent = 1.0866012754,
  wealthToPopulationExponent = 0.3804356044
)

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

object NationalRedistributionBonusTest extends NationalRedistributionBonusModel(
  bonusMultiplier = 427.5248642847163,
  fixedCost = 283.3037716981452,
  distanceDecay = 5.163495482844931,
  sizeEffectOnSupply = 1.5414957182929494,
  sizeEffectOnDemand = 9.814965767224715,
  economicMultiplier = 0.0,
  populationToWealthExponent = 1.389243762417773,
  wealthToPopulationExponent = 0.9143309383981182,
  territorialTaxes = 0.04106670424962933,
  capitalShareOfTaxes = 0.0
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
  territorialTaxes = 0.0,
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
  territorialTaxes = 0.001,
  capitalShareOfTaxes = 0.001,
  oilAndGazEffect = 0.03,
  coalEffect = -0.04
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

    out.append("step, arokato, population, wealth, supply, demand, transactedFrom, transactedTo \n")

    for {
      (log, step) <- m.logs zipWithIndex
    } log.value match {
      case Success(s) =>
        val cs = s |-> cities get
        val transacted = log.written
        val from = transacted.groupBy(_.from)
        val to = transacted.groupBy(_.to)

        for {
          (city, arokato, s, d, i) <- (
            cs zip
            MariusFile.arokatos zip
            m.supplies(cs) zip
            m.demands(cs)).zipWithIndex.map(flatten)
        } {
          def line =
            Seq(
              step,
              arokato,
              city |-> population get,
              city |-> wealth get,
              s,
              d,
              from.getOrElse(i, Seq.empty).map(_.transacted).sum,
              to.getOrElse(i, Seq.empty).map(_.transacted).sum
              )
          out.append(line.mkString(",") + "\n")
        }
        val totalWealth = cs.map(_ |-> wealth get).sum
        val totalPop = cs.map(_ |-> population get).sum

        println(s"step $step, total wealth $totalWealth, total population $totalPop")


      case Failure(e) => println(s"Invalid State $e")
    }
  }

}
