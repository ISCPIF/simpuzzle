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

package fr.geocites.sugarscape

import scala.io.Source
import scala.util.Random
import fr.geocites.simpuzzle.distribution._

trait SugarScapeInitialState <: SugarScapeState {

  def nbAgents: Int

  def initialSugarDistribution = new UniformDoubleDistribution {
    def max = 25
    def min = 5
  }

  def initialMetabolismDistribution = new UniformDoubleDistribution {
    def max = 4
    def min = 1
  }

  def initialVisionDistribution = new UniformIntDistribution {
    def max = 6
    def min = 1
  }

  def initialAgents(implicit rng: Random) =
    (initialSugarDistribution.iterator zip
      initialMetabolismDistribution.iterator zip
      initialVisionDistribution.iterator) map {
        case ((s, m), v) => Agent(s, m, v)
      }

  def initial(implicit rng: Random) = {
    val cells =
      readLandscape.map { l => l.map { sugar => Sugar(sugar, sugar) } }

    val positions =
      maxSugarCells.zipWithIndex.flatMap {
        case (l, i) => l.zipWithIndex.map {
          case (_, j) => i -> j
        }
      }

    val agentPositions = rng.shuffle(positions) zip initialAgents.toSeq
    SugarScapeState(0, agentPositions, cells)
  }

  def readLandscape = {
    val input =
      Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream("sugar-map.txt"))

    input.getLines().map {
      _.split(" ").map { _.toInt }.toSeq
    }.toSeq
  }

}
