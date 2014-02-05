/*
 * Copyright (C) 10/01/14 Romain Reuillon
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

import org.graphstream.graph.implementations.MultiGraph
import fr.geocite.marius._
import fr.geocite.marius.matching._
import fr.geocite.simpuzzle._

object MariusGraphStream extends App {

  val m = new Marius with MariusState with ProportionalMatching {
    def distanceDecay = 1
    def sizeEffectOnConsumption = 142
    def sizeEffectOnProductivity = 300
    def gamma = 92
    def territorialTaxes = 0.0
    def capitalShareOfTaxes = 0.0
    def distanceOrderSell = 0.854448839836991
    def inversionPoint = 10.00029163524558
    def maxStep = 31
  }

  implicit val rng = fr.geocite.simpuzzle.random(42)

  for {
    state <- m.states
  } {
    val cities = state.value.cities
    val transacted = state.written

    println(transacted.size)

    /* for {
      (cities, rokato, name, lat, long, i) <- (cities zip m.rokato zip m.names zip m.lat zip m.long).zipWithIndex.map(flatten)
    } {

    }  */
  }

  val graph = new MultiGraph("Marius")

  graph.addNode("A")
  graph.addNode("B")
  graph.addNode("C")
  graph.addEdge("AB", "A", "B")
  graph.addEdge("BC", "B", "C")
  graph.addEdge("CA", "C", "A")

  graph.display()

}
