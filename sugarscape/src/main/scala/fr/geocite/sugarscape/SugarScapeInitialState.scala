/*
 * Copyright (C) 09/09/13 Romain Reuillon
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

package fr.geocite.sugarscape

import scala.io.Source
import scala.util.Random

trait SugarScapeInitialState <: SugarScapeState {

  lazy val maxSugarCells = readLandscape

  def initial(implicit rng: Random) = SugarScapeState(0, maxSugarCells)

  def readLandscape = {
    val input =
      Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream("sugar-map.txt"))

    input.getLines().map {
      _.split(" ").map { _.toInt }.toSeq
    }.toSeq
  }

}
