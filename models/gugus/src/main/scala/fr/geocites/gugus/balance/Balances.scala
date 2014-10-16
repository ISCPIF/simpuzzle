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

import scala.util.Random
import fr.geocites.simpuzzle._

trait Balances <: Exchange { model: Gugus =>

  def balances(s: STATE,
    supplies: Seq[Double],
    demands: Seq[Double])(implicit rng: Random) = {
    for {
      exchangeBalance <- exchangeBalances(s, supplies.toIndexedSeq, demands.toIndexedSeq)
    } yield {
      (exchangeBalance zip redistributionBalances(cities.get(s))).map(flatten).map {
        case (exchangeBalance, redistributionBalance) => exchangeBalance + redistributionBalance
      }
    }
  }
  def redistributionBalances(s: Seq[CITY]) = s.map(_ => 0.0)
}

