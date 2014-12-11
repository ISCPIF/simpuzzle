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

package fr.geocites.simpoplocal

import fr.geocites.simpuzzle.logging.Log

import scala.util.Random

object Run extends App {

  val m = new SimpopLocal with Log {
    def distanceDecay: Double = 0.6882107473716844
    def innovationImpact: Double = 0.007879556611500305
    def maxInnovation: Double = 10000
    def pCreation: Double = 1.2022185310640896E-6
    def pDiffusion: Double = 7.405303653131592E-7
    def rMax: Double = 10259.331894632433
  }

  implicit val rng = new Random(42)

  val exchanges =
    (for {
      s <- m.logs
    } yield {
      s.written.size
    }).sum

  println(exchanges)

}
