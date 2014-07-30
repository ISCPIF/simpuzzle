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
  bonusMultiplier = 566.040564661515,
  fixedCost = 0,
  distanceDecay = 0,
  sizeEffectOnSupply = 1.2220357365531,
  sizeEffectOnDemand = 1.18530440441915,
  economicMultiplier = 0.589041240155966,
  populationToWealthExponent = 1,
  wealthToPopulationExponent = 0.906444352884275
)


object MariusCSV extends App {

  lazy val models = List(BonusFixedCostTest)

  println(Console.YELLOW + "Choose you model: ")
  models.map(_.getClass.getName).zipWithIndex.foreach{ case(c, i) => println(Console.GREEN + s"$i -> ${Console.WHITE} $c") }
  val i = io.StdIn.readInt()
  print(Console.WHITE)

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
