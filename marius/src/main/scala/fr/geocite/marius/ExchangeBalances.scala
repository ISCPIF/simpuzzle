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

package fr.geocite.marius

import fr.geocite.marius.matching.Matching
import scala.util.Random
import fr.geocite.simpuzzle._

trait ExchangeBalances <: Matching with MariusLogging {

  def bonusMultiplier: Double

  def exchangeBalances(
    s: STATE,
    supplies: Seq[Double],
    demands: Seq[Double])(implicit rng: Random) = {

    val transactions = matchCities(s, supplies, demands)

    val transposedTransactions = transactions.transpose

    def unsatisfieds =
      for {
        (d, i) <- demands.zipWithIndex
        transactionsTo = transposedTransactions(i)
      } yield {
        val transactedSum = transactionsTo.map(_.transacted).sum
        val unsatisfied = d - transactedSum
        check(unsatisfied >= 0, s"unsatisfied not good city $i , unsat $unsatisfied demand  $d  sum transac $transactedSum ")
        unsatisfied
      }

    def unsolds =
      for {
        (s, i) <- supplies.zipWithIndex
        transactionsFrom = transactions(i)
      } yield {
        val unsold = s - transactionsFrom.map(_.transacted).sum
        check(unsold >= 0, s"unsold not good, $unsold")
        unsold
      }

    def importShares =
      for {
        (demand, i) <- demands.zipWithIndex
        transactionsTo = transposedTransactions(i)
      } yield transactionsTo.map(_.transacted).sum / demand

    def exportShares =
      for {
        (supply, i) <- supplies.zipWithIndex
        transactionsFrom = transactions(i)
      } yield transactionsFrom.map(_.transacted).sum / supply

    def balances = (unsolds zip unsatisfieds zip importShares zip exportShares).map(flatten).map {
      case (unsold, unsatisfied, importShare, exportShare) =>
        val newWealth = unsold + unsatisfied + (bonusMultiplier * (importShare + exportShare))
        if (newWealth <= 0.0) 0.0 else newWealth
    }
    log(balances, transactions.flatten)
  }

}
