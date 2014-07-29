/*
 * Copyright (C) 23/10/13 Romain Reuillon
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

package fr.geocites.marius.transaction

import scala.util.Random
import fr.geocites.marius.{ Interaction, Marius }
import scala.math._
import fr.geocites.simpuzzle._
import fr.geocites.marius.structure.Matrix._
import fr.geocites.marius.structure.Network

trait ProportionalTransaction <: Transaction
    with PotentialMatrix
    with Marius { this: Marius =>

  def transactions(
    state: STATE,
    supplies: Seq[Double],
    demands: Seq[Double])(implicit rng: Random) = {

    val indexedSupplies = supplies.toIndexedSeq
    val indexedDemands = demands.toIndexedSeq

    val interactionMatrixValue = interactionPotentialMatrix(state, indexedSupplies, indexedDemands)
    val fromInteractionPotentialSum = interactionMatrixValue.linesContent.map(_.sum)
    val toInteractionPotentialSum = interactionMatrixValue.transpose.linesContent.map(_.sum)

    interactionMatrixValue.map {
      (from, to, ip) =>
        if (ip > 0) {
          val fSupply = indexedSupplies(from)
          val tDemand = indexedDemands(to)
          val toIPSum = toInteractionPotentialSum(to)
          val fromIPSum = fromInteractionPotentialSum(from)
          check(fSupply >= 0 && tDemand >= 0, s"supply or demand not good, $fSupply $tDemand")

          val normalisedIPFrom = ip / fromIPSum
          val normalisedIPTo = ip / toIPSum

          val t = min(normalisedIPFrom * fSupply, normalisedIPTo * tDemand)
          check(
            !t.isNaN, s"Transacted is NaN: from $from to $to , ip%from : $normalisedIPFrom supplyfrom  $fSupply todemand $tDemand ip%to $normalisedIPTo  fromipsum $fromIPSum toipsum $toIPSum",
            PotentialMatrix.InteractionPotentialException(_, interactionMatrixValue)
          )
          t
        } else 0.0
    }
  }

}
