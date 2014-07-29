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

object TestModel extends BonusFixedCostModel(
  bonusMultiplier = 39.1721815636,
  fixedCost = 0.0,
  distanceDecay = 3.4904055398,
  sizeEffectOnSupply = 1.2010954748,
  sizeEffectOnDemand = 1.9538793197,
  economicMultiplier = 0.0001913588,
  populationToWealthExponent = 1.4874860549,
  wealthToPopulationExponent = 0.9160543414
)


object MariusCSV extends App {

  val m = TestModel

  implicit val rng = fr.geocites.simpuzzle.random(42)

  val path = "/tmp/mariusmodel_log.csv"

  new File (path).delete

  val out = Resource.fromFile(path)

  out.append("step, arokato, population, wealth \n")

  for {
    (log, cptr) <- m.logs zipWithIndex
  } log.value match {
    case Success(s) =>
      val cities = s.cities
      val transacted = log.written

      for {
        (city, arokato, i) <- (cities zip MariusFile.arokatos).zipWithIndex.map(flatten)
      } {
        def line = Seq(cptr, arokato, city.population, city.wealth)
        out.append(line.mkString("", ",", "\n"))

      }
      val totalWealth = cities.map(_.wealth).sum
      val totalPop = cities.map(_.population).sum

      println("Etat ", cptr, " Wealth totale", totalWealth, " pop totale", totalPop)
    case Failure(e) => println(s"Invadid State $e")
  }

}
