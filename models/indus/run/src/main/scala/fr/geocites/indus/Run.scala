/*
 * Copyright (C) 2014 Romain Reuillon
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

package fr.geocites.indus

import java.io.File

import Models._
import fr.geocites.gugus.tool.ToCSV

import scala.util.Random

object Run extends App with ToCSV {
  val model =
    SimpleModel(
      distanceDecay = 0.6722631615,
      sizeEffectOnSupply = 1.001756388,
      sizeEffectOnDemand = 1.0792607803,
      economicMultiplier = 0.3438093442,
      populationToWealthExponent = 1.0866012754,
      wealthToPopulationExponent = 0.3804356044)

  implicit val rng = new Random(42)

  val path = new File("/tmp/indusmodel_log.csv")

  toCSVFile(model, path)

}
