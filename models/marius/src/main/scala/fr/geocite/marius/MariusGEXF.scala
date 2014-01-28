/*
 * Copyright (C) 07/01/14 Romain Reuillon
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

import fr.geocite.marius.one._
import matching._
import java.util.Calendar
import scalax.io.Resource
import fr.geocite.simpuzzle._

object MariusGEXF extends App {

  val m = new Marius with Basic with ProportionalMatching {
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

  def transactedCeil = 100

  val graph = Resource.fromFile("/tmp/marius.gexf")

  graph.append(
    """<?xml version="1.0" encoding="UTF-8"?>
<gexf xmlns="http://www.gexf.net/1.2draft" xmlns:viz="http://www.gexf.net/1.1draft/viz" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.gexf.net/1.2draft http://www.gexf.net/1.2draft/gexf.xsd" version="1.2">
    <meta>
        <creator>Marius</creator>
        <description>Marius model run</description>
    </meta>
    <graph mode="dynamic" defaultedgetype="directed">
        <attributes class="node" mode="static">
            <attribute id="rokato" title="rokato" type="string"/>
            <attribute id="name" title="name" type="string"/>
        </attributes>
        <attributes class="node" mode="dynamic">
            <attribute id="population" title="population" type="double"/>
            <attribute id="wealth" title="wealth" type="double"/>
        </attributes>
        <attributes class="edge" mode="dynamic">
            <attribute id="transacted" title="transacted" type="double"/>
            <attribute id="weight" title="Weight" type="float"/>
        </attributes>""")

  implicit val rng = fr.geocite.simpuzzle.random(42)
  val states = m.states.toSeq

  graph.append("<nodes>\n")

  def citiesStates =
    states.map {
      s => s.value.cities.toSeq
    }.transpose

  for {
    (cityStates, rokato, name, lat, long, i) <- (citiesStates zip m.rokato zip m.names zip m.lat zip m.long).zipWithIndex.map(flatten)
  } {

    def attributes =
      cityStates.zipWithIndex.flatMap {
        case (s, step) =>
          Seq(
            s"""<attvalue for="population" value="${s.population}" start="$step" end="${step + 1}"/>""",
            s"""<attvalue for="wealth" value="${s.wealth}" start="$step" end="${step + 1}"/>""",
            s"""<viz:position x="$long" y="$lat" z="0.0"/>"""
          )
      }

    graph.append(
      s"""<node id="$i" label="$rokato">
                <attvalues>
                    <attvalue for="rokato" value="$rokato"/>
                    <attvalue for="name" value="$name"/>
                    ${attributes.mkString("\n")}
                </attvalues>
            </node>""")
  }

  graph.append("</nodes>\n")
  graph.append("<edges>\n")

  def links = {
    val res = Array.ofDim[Double](m.nbCities, m.nbCities, states.size)

    for {
      (s, step) <- states.zipWithIndex
      t <- s.written
    } res(t.from)(t.to)(step) = t.transacted

    res
  }

  for {
    (x, from) <- links.zipWithIndex
    (transacted, to) <- x.zipWithIndex
    if transacted.exists(_ > transactedCeil)
  } {
    def attributes =
      transacted.zipWithIndex.flatMap {
        case (t, step) =>
          Seq(
            s"""<attvalue for="transacted" value="$t" start="$step" end="${step + 1}"/>""",
            s"""<attvalue for="weight" value="$t" start="$step" end="${step + 1}"/>"""
          )
      }

    graph.append(
      s"""<edge source="$from" target="$to">
                <attvalues>
                    ${attributes.mkString("\n")}
                </attvalues>
            </edge>""")
  }

  graph.append("</edges>\n")
  graph.append("""      </graph>
</gexf>""")
}

