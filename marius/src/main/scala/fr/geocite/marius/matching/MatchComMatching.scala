///*
// * Copyright (C) 23/10/13 Romain Reuillon
// *
// * This program is free software: you can redistribute it and/or modify
// * it under the terms of the GNU Affero General Public License as published by
// * the Free Software Foundation, either version 3 of the License, or
// * (at your option) any later version.
// *
// * This program is distributed in the hope that it will be useful,
// * but WITHOUT ANY WARRANTY; without even the implied warranty of
// * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// * GNU Affero General Public License for more details.
// *
// * You should have received a copy of the GNU General Public License
// * along with this program.  If not, see <http://www.gnu.org/licenses/>.
// */
//
//package fr.geocite.marius.matching
//
//import scala.util.Random
//import fr.geocite.simpuzzle.distribution._
//import fr.geocite.marius.{ Transaction, Marius }
//
////FIXME city too poor assertion fail after 2 steps
//trait MatchComMatching <: Matching with InteractionPotential with Marius {
//
//  /**
//   * Relative importance of distance in the choice of partners in according to the interaction potential
//   * distanceOrderBuy gives more importance to Distance to select potential buyers
//   * distanceOrderSell gives more importance to Supply to select potential sellers
//   * so distanceOrderBuy should be higher than distanceOrderSell
//   */
//  def distanceOrderBuy: Double
//
//  def distanceOrderSell: Double
//
//  def partnerMultiplier: Double
//
//  def matchCities(
//    s: STATE,
//    supplies: Seq[Double],
//    demands: Seq[Double])(implicit rng: Random) = {
//    val potentialBuyers: Seq[Set[Int]] = potentialBuyerNetwork(cities.get(s), distanceMatrix.get(s), supplies, demands)
//    val potentialSellers: Seq[Set[Int]] = potentialSellerNetwork(cities.get(s), distanceMatrix.get(s), supplies, demands)
//
//    val sellContracts: Seq[Set[Int]] = contracts(potentialBuyers, potentialSellers)
//    val buyContract: Seq[Set[Int]] = contracts(potentialSellers, potentialBuyers)
//
//    val propositionToSell: Seq[Map[Int, Double]] =
//      (sellContracts zip supplies).map {
//        case (buyers, supply) =>
//          reservations(buyers.toSeq, demands, supply)
//      }
//
//    val propositionToBuy: Seq[Map[Int, Double]] =
//      (buyContract zip demands).map {
//        case (sellers, demand) =>
//          reservations(sellers.toSeq, supplies, demand)
//      }
//
//    case class TransactionPotential(from: Int, to: Int, pps: Double, ppb: Double) {
//      def toTransaction = Transaction(from, to, transacted)
//      def transacted = math.min(pps, ppb)
//    }
//
//    def computeTransactions(from: Int, ppss: Map[Int, Double]) =
//      ppss.toSeq.map {
//        case (to, pps) =>
//          val ppb = propositionToBuy(to).getOrElse(from, sys.error(s"Transaction is empty $from to $to"))
//          assert(!ppb.isNaN)
//          assert(!pps.isNaN)
//          TransactionPotential(from, to, pps, ppb)
//      }
//
//    val transactions = propositionToSell.zipWithIndex.flatMap {
//      case (ppss, city) => computeTransactions(city, ppss)
//    }.toSeq
//
//    val transactedFrom: Map[Int, Seq[TransactionPotential]] =
//      transactions.groupBy(_.from).withDefaultValue(Seq.empty)
//
//    val transactedTo: Map[Int, Seq[TransactionPotential]] =
//      transactions.groupBy(_.to).withDefaultValue(Seq.empty)
//
//    def unsolds =
//      for {
//        cid <- 0 until cities.get(s).size
//        transaction = transactedFrom(cid)
//      } yield transaction.map(_.pps).sum - transaction.map(_.transacted).sum
//
//    def unsatisfieds =
//      for {
//        cid <- 0 until cities.get(s).size
//        transaction = transactedTo(cid)
//      } yield transaction.map(_.ppb).sum - transaction.map(_.transacted).sum
//
//    def importShares =
//      for {
//        cid <- 0 until cities.get(s).size
//        transaction = transactedTo(cid)
//      } yield transaction.map(_.transacted).sum // à diviser par la demande de la ville
//
//    def exportShares =
//      for {
//        cid <- 0 until cities.get(s).size
//        transaction = transactedFrom(cid)
//      } yield transaction.map(_.transacted).sum // à diviser par la supply de la ville
//
//    Matched(transactions.map(_.toTransaction), unsolds, unsatisfieds, importShares, exportShares)
//  }
//
//  def contracts(from: Seq[Set[Int]], to: Seq[Set[Int]]) =
//    from.zipWithIndex.map {
//      case (pTo, city) =>
//        pTo.filter { to(_).contains(city) }
//    }
//
//  def reservations(requester: Seq[Int], requested: Seq[Double], toShare: Double): Map[Int, Double] = {
//    val effectiveRequests = requester.map(requested)
//    val totalRequest = effectiveRequests.sum
//    if (totalRequest <= 0) Map.empty
//    else {
//      val fractions = effectiveRequests.map(_ / totalRequest)
//      (requester zip fractions.map(_ * toShare)).toMap
//    }
//  }
//
//  def potentialBuyerNetwork(
//    s: Seq[CITY],
//    distances: Seq[Seq[Double]],
//    supplies: Seq[Double],
//    demands: Seq[Double])(implicit rng: Random) = {
//    val interactionPotentials = interactionPotentialMatrix(s, supplies, distances)
//    (s zip demands).zipWithIndex.map {
//      case ((c1, d1), i) =>
//        drawCandidates(interactionPotentials(i).zipWithIndex, distances, supplies, s => s >= partnerMultiplier * d1)
//    }
//  }
//
//  def potentialSellerNetwork(
//    s: Seq[CITY],
//    distances: Seq[Seq[Double]],
//    supplies: Seq[Double],
//    demands: Seq[Double])(implicit rng: Random) = {
//    val acquaintance = interactionPotentialMatrix(s, supplies, distances)
//    (s zip supplies).zipWithIndex.map {
//      case ((c1, s1), i) =>
//        drawCandidates(acquaintance(i).zipWithIndex, distances, supplies, s => s >= partnerMultiplier * s1)
//    }
//  }
//
//  def drawCandidates(
//    weighted: Seq[(Double, Int)],
//    distances: Seq[Seq[Double]],
//    otherQuantity: Seq[Double],
//    satisfied: Double => Boolean)(implicit rng: Random) = {
//    def drawOneCandidate(candidates: List[(Double, Int)], selected: List[Int] = List.empty, totalQuantity: Double = 0): Set[Int] =
//      if (candidates.isEmpty || satisfied(totalQuantity)) selected.toSet
//      else {
//        val (s, remaining) = multinomialDraw(candidates)
//        drawOneCandidate(remaining, s :: selected, totalQuantity + otherQuantity(s))
//      }
//    drawOneCandidate(weighted.toList)
//  }
//
//  def matchNetwork(potentialBuyers: Seq[Int], potentialSellers: Seq[Int]) =
//    potentialBuyers.toSet & potentialSellers.toSet
//
//}
