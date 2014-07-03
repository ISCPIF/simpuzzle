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

trait TargetDistribution {

  def date(step: Int) = MariusFile.dates.head + step

  def distribution(marius: Marius with MariusFile with MariusCity)(implicit rng: Random) = Try {
    val fitness = (for { (state, step) <- marius.states.zipWithIndex} yield {
      state match {
        case marius.ValidState(s) =>
          val deadCities = marius.cities.get(s).count(_.wealth <= 0.0)

          marius.populations(MariusFile.dates.head + step).map {
            empirical =>
              val distance = statistics.logSquaresError(marius.cities.get(s).map(_.population).sorted, empirical.sorted)
              Seq(deadCities, distance)
          }.getOrElse(Seq(deadCities, 0.0))
        case marius.InvalidState(_) => Seq(Double.PositiveInfinity, Double.PositiveInfinity)
      }
    }).foldLeft(Seq(0.0, 0.0)){ (s, v) => (s zip v).map{ case(x, y) => x + y } }

    fitness.map(x => if(x.isNaN) Double.PositiveInfinity else x)
  }.getOrElse(Seq(Double.PositiveInfinity, Double.PositiveInfinity))

}
