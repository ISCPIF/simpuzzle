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

package fr.geocites.gugus.balance

import fr.geocites.gugus.Gugus

trait Redistribution <: Balances with Gugus {

  type RedistributionFunction = (Seq[CITY] => Seq[Double])

  def redistributions: Seq[RedistributionFunction]

  override def redistributionBalances(s: Seq[CITY]): Seq[Double] =
    redistributions.foldLeft(s.map(_ => 0.0)) {
      (total, r) => (total zip r(s)).map { case (t, r) => t + r }
    }

}
