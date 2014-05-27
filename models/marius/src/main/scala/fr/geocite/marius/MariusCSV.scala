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

package fr.geocite.marius

import fr.geocite.marius._
import fr.geocite.marius.matching._
import fr.geocite.simpuzzle._
import scalax.io.Resource
import fr.geocite.simpuzzle.logging.NoLogging
import fr.geocite.marius.state._

object MariusCSV extends App {

  val m = new Marius with NetworkState with FixedCostMatching with NoLogging {

    def popMax: Double = 11005.3173124776
    def popMin: Double = 0
    def wMax: Double = 195414.92562309
    def wMin: Double = 0
    def territorialTaxes = 0.0
    def capitalShareOfTaxes = 0.0
    def consumptionProductivityRatio: Double = 4.770560411778249
    def distanceDecay = 4.4007451235
    def inversionPoint = 179.0416594111
    def sizeEffectOnProductivity = 0.0623952569
    def sizeEffectOnConsumption = 0.0050156088

    def fixedCost = 100.0
    override def networkShare: Double = 0.01

    def maxStep = 30
  }

  implicit val rng = fr.geocite.simpuzzle.random(42)

  //(0 until 10).foreach { i => println(i); m.run }

  val path = "/tmp/mariusmodel_log.csv"

  val out = Resource.fromFile(path)

  out.append("step, arokato, population, wealth \n")

  for {
    (log, cptr) <- m.logs zipWithIndex
  } log.value match {
    case m.ValidState(s) =>
      val cities = s.cities
      val transacted = log.written

      for {
        (city, rokato, name, lat, long, i) <- (cities zip m.rokato zip m.names zip m.lat zip m.long).zipWithIndex.map(flatten)
      } {
        def uneligne = Seq(cptr, rokato, city.population, city.wealth)
        out.append(uneligne.mkString("", ",", "\n"))

      }
      val totalWealth = cities.map(_.wealth).sum
      val totalPop = cities.map(_.population).sum

      println("Etat ", cptr, " Wealth totale", totalWealth, " pop totale", totalPop)
    case m.InvalidState(e) => println(s"Invadid State $e")
  }

}
