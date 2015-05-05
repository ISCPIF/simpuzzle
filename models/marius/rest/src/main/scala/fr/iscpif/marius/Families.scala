/*
 * Copyright (C) 2015 Romain Reuillon
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
package fr.iscpif.marius

import java.io.StringWriter

import fr.geocites.gugus.balance.Bonus
import fr.geocites.gugus.transaction.FixedCostTransaction
import fr.geocites.gugus.urbanisation.UrbanTransition
import fr.geocites.marius.{ From1989To2010, From1959To1989, DoubleRedistribution, SubSurfaceResources }
import fr.iscpif.family._

object Families {

  lazy val marius1 = new ModelFamily with ScriptEngineCompilation {
    def imports: Seq[String] = Seq(
      "java.io._",
      "fr.geocites.gugus.tool._",
      "fr.geocites.gugus.transaction._",
      "fr.geocites.marius._")
    def source(traits: String, attributes: String): String =
      s"""
        |val model =
        |  new Marius with ProportionalTransaction with $traits {
        |    $attributes
        |  }
        |val csvWriter = new StringWriter()
        |ToCSV.toCSV(model, csvWriter)
        |val csv = csvWriter.toString""".stripMargin

    def inputs = Seq.empty
    def attributes =
      Seq(
        "economicMultiplier",
        "populationToWealthExponent",
        "sizeEffectOnSupply",
        "sizeEffectOnDemand",
        "wealthToPopulationExponent",
        "distanceDecay",
        "bonusMultiplier",
        "fixedCost",
        "oilAndGazEffect",
        "coalEffect",
        "territorialTaxes",
        "capitalShareOfTaxes",
        "ruralMultiplier"
      ).map(TypedValue[Double])
    def outputs = Seq(TypedValue[String]("csv"))
    def combination: Combination[Class[_]] =
      AllToAll(
        AnyOf(classOf[Bonus], classOf[FixedCostTransaction], classOf[SubSurfaceResources], classOf[DoubleRedistribution], classOf[UrbanTransition]),
        OneOf(classOf[From1959To1989], classOf[From1989To2010])
      )
  }

  def all = Map("marius1" -> marius1)

  all.values.foreach(_.compiled.get)

}
