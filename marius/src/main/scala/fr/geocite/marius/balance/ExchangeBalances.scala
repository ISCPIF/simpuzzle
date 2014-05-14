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

package fr.geocite.marius.balance

import fr.geocite.marius.matching.Matching
import scala.util.Random
import fr.geocite.simpuzzle._
import fr.geocite.marius._

trait ExchangeBalances <: Matching with NoBonus { this: Marius =>

  def exchangeBalances(
    s: STATE,
    supplies: Seq[Double],
    demands: Seq[Double])(implicit rng: Random) = {

    val transacted = matchCities(s, supplies, demands)
    val transposedTransacted = transacted.transpose

    def unsatisfieds =
      for {
        (d, i) <- demands.zipWithIndex
        transactionsTo = transposedTransacted(i)
      } yield {
        val transactedSum = transactionsTo.sum
        val unsatisfied = d - transactedSum
        if (unsatisfied >= 0) unsatisfied else 0
      }

    def unsolds =
      for {
        (s, i) <- supplies.zipWithIndex
        transactionsFrom = transacted(i)
      } yield {
        val unsold = s - transactionsFrom.sum
        if (unsold >= 0) unsold else 0
      }

    def importShares =
      for {
        (demand, i) <- demands.zipWithIndex
        transactionsTo = transposedTransacted(i)
      } yield transactionsTo.sum / demand

    def exportShares =
      for {
        (supply, i) <- supplies.zipWithIndex
        transactionsFrom = transacted(i)
      } yield transactionsFrom.sum / supply

    def diversityBonuses = {
      def transactedWith(transacted: Seq[Double]) =
        transacted.zipWithIndex.filter { case (v, _) => v > 0 }.unzip._2

      (transacted zip transposedTransacted) map {
        case (from, to) =>
          (transactedWith(from).toSet union transactedWith(to).toSet).size / cities.get(s).size.toDouble
      }
    }

    def balances = (unsolds zip unsatisfieds zip bonuses(importShares, exportShares, diversityBonuses)).map(flatten).map {
      case (unsold, unsatisfied, bonus) => unsatisfied - unsold + bonus
    }

    def transactions =
      for {
        (l, i) <- transacted.zipWithIndex
        (v, j) <- l.zipWithIndex
      } yield Transaction(i, j, v)

    log(balances, transactions)
  }

}
