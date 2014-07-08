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


package fr.geocites.marius.target

import fr.geocite.marius.state.MariusCity
import fr.geocite.marius.{Marius, MariusFile}
import scala.util.Random
import util.Try
import monocle.syntax._

trait TargetDistribution {

  def date(step: Int) = MariusFile.dates.head + step

  def distribution(marius: Marius with MariusFile with MariusCity)(implicit rng: Random) = Try {

    def totalOverflowRatio(cities: Seq[marius.CITY]) =
      cities.map {
        c =>
          overflowRatio(c.wealth, marius.supply(c.population)) +  overflowRatio(c.wealth, marius.demand(c.population))
      }.sum

    def overflowRatio(wealth: Double, flow: Double) = {
      val ratio = flow / wealth
      if(ratio < 1.0) 0.0 else ratio - 1
    }

    val fitness = (for { (state, step) <- marius.states.zipWithIndex} yield {
      state match {
        case marius.ValidState(s) =>
          val overflow = totalOverflowRatio(s |-> marius.cities get)

          val deadCities = (s |-> marius.cities get).count(_.wealth <= 0.0)

          val distance = marius.populations(MariusFile.dates.head + step).map {
            empirical => statistics.logSquaresError(marius.cities.get(s).map(_.population).sorted, empirical.sorted)
          }.getOrElse(0.0)

          Seq(deadCities, distance, overflow)
        case marius.InvalidState(_) => Seq(Double.PositiveInfinity, Double.PositiveInfinity, Double.PositiveInfinity)
      }
    }).foldLeft(Seq(0.0, 0.0, 0.0)){ (s, v) => (s zip v).map{ case(x, y) => x + y } }

    fitness.map(x => if(x.isNaN) Double.PositiveInfinity else x)
  }.getOrElse(Seq(Double.PositiveInfinity, Double.PositiveInfinity, Double.PositiveInfinity))




}
