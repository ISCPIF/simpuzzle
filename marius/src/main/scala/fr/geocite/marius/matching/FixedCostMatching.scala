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

package fr.geocite.marius.matching

import scala.util.Random
import fr.geocite.marius._
import structure.Matrix._
import scala.collection.mutable.ListBuffer
import fr.geocite.marius.structure.SparseMatrix

trait FixedCostMatching <: Matching with InteractionPotential { this: Marius =>

  def fixedCost: Double

  override def matchCities(
    s: STATE,
    supplies: Seq[Double],
    demands: Seq[Double])(implicit rng: Random) = {

    def splitTheCake(neighbours: Seq[(Int, Double)], cakeSize: Double) = {
      val nPI = neighbours.unzip._2
      val totalNeighboursPI = nPI.sum
      val relativeNeighboursPI = nPI.map { _ / totalNeighboursPI }
      (neighbours.unzip._1 zip relativeNeighboursPI).map { case (n, pi) => (n, pi, pi * cakeSize) }
    }

    val supplied =
      for {
        (city, cityId) <- cities.get(s).zipWithIndex
      } yield {
        val outNeighbours = network.get(s).outNodes(cityId)

        // Interaction potential is not symmetric but has the same expression for both ways
        val neighboursPI =
          outNeighbours.map {
            neighbourId =>
              interactionPotential(supplies(cityId), demands(neighbourId), distanceMatrix(cityId)(neighbourId))
          }.toIndexedSeq

        val supply = supplies(cityId)

        val viableTransactingNeighbours = splitTheCake(outNeighbours zip neighboursPI, supply).filter { case (_, _, r) => r >= fixedCost }
        val supplied = splitTheCake(
          viableTransactingNeighbours.map { case (n, pi, _) => (n, pi) },
          supply)

        supplied.map { case (neighbour, pi, supplied) => (neighbour, pi, supplied) }
      }

    case class SuppliedToDestination(from: Int, interactionPotential: Double, supplied: Double)

    val suppliedIndexedByDestination = Vector.fill(cities.get(s).size)(ListBuffer[SuppliedToDestination]())
    for {
      (from, to, supply, pi) <- supplied.zipWithIndex.map { case (s, from) => s.map { case (n, pi, s) => (from, n, pi, s) } }.flatten
    } suppliedIndexedByDestination(to) += SuppliedToDestination(from, pi, supply)

    val demanded =
      for {
        (supplied, cityId) <- suppliedIndexedByDestination.zipWithIndex
      } yield {
        splitTheCake(supplied.map(s => (s.from, s.interactionPotential)), demands(cityId)).map { case (n, _, d) => (n, d) }
      }

    val cells =
      for {
        (suppliesForCity, demandsForCity) <- supplied zip demanded
      } yield {
        def generateCells(supplies: List[(Int, Double, _)], demands: List[(Int, Double)]): List[Cell] =
          (supplies, demands) match {
            case (Nil, _) => Nil
            case (_, Nil) => Nil
            case ((ns, qs, _) :: tails, (nd, qd) :: taild) =>
              if (ns == nd) Cell(ns, math.min(ns, nd)) :: generateCells(tails, taild)
              else if (ns < nd) generateCells(tails, demands)
              else generateCells(supplies, taild)
          }

        generateCells(suppliesForCity.toList, demandsForCity.toList)
      }

    SparseMatrix(cells)

  }
}
