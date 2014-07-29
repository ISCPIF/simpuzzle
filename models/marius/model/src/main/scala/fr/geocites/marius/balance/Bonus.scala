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

package fr.geocites.marius.balance

import fr.geocites.marius.structure.{ Matrix, Network }
import fr.geocites.marius.structure.Matrix.Cell
import fr.geocites.marius.Marius
import fr.geocites.simpuzzle._

trait Bonus <: Exchange { marius: Marius =>
  def bonusMultiplier: Double

  override def bonuses(t: Transacted): Seq[Double] = {
    def diversityBonuses = {
      def transactedWith(transacted: Seq[Cell]) =
        transacted.filter { case Cell(_, v) => v > 0 }.map { case Cell(to, _) => to }

      (t.transacted.lines zip t.transposedTransacted.lines) map {
        case (from, to) =>
          (transactedWith(from).toSet union transactedWith(to).toSet).size / t.nbCities.toDouble
      }
    }

    def importVolumes =
      for {
        (demand, i) <- t.demands.zipWithIndex
      } yield t.transactedToSum(i)

    def exportVolumes =
      for {
        (supply, i) <- t.supplies.zipWithIndex
      } yield t.transactedFromSum(i)

    (importVolumes zip exportVolumes zip diversityBonuses).map(flatten) map {
      case (is, es, diversityBonus) =>
        bonusMultiplier * (is + es) * diversityBonus
    }
  }

}
