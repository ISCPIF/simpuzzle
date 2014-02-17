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
import scalax.file.Path

object MariusCSV extends App {

  val m = new Marius with MariusState with ProportionalMatching {
    def distanceDecay = 1.1

    def sizeEffectOnConsumption = 0.000002
    def sizeEffectOnProductivity = 0.0035
    def gamma = 0
    def territorialTaxes = 0.0
    def capitalShareOfTaxes = 0.0

    def maxStep = 30
  }

  implicit val rng = fr.geocite.simpuzzle.random(42)

  val path = "/tmp/mariusmodel_log.csv"

  val out = Resource.fromFile(path)

  out.append("step, arokato, population, wealth \n")

  for {
    (state, cptr) <- m.states zipWithIndex
  } {
    val cities = state.value.cities
    val transacted = state.written

    for {
      (city, rokato, name, lat, long, i) <- (cities zip m.rokato zip m.names zip m.lat zip m.long).zipWithIndex.map(flatten)
    } {
      def uneligne = Seq(cptr, rokato, city.population, city.wealth)
      out.append(uneligne.mkString("", ",", "\n"))

    }
    val totalWealth = cities.map(_.wealth).sum
    val totalPop = cities.map(_.population).sum

    println("Etat ", cptr, " Wealth totale", totalWealth, " pop totale", totalPop)

  }

}
