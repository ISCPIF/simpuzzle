package fr.geocites.marius.target

import fr.geocite.marius.state.MariusCity
import fr.geocite.marius.{Marius, MariusFile}

import scala.util.Random

/*
 * Copyright (C) 01/12/13 Romain Reuillon
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

trait Target {
  def date(step: Int) = MariusFile.dates.head + step
  def distribution(marius: Marius with MariusFile with MariusCity)(implicit rng: Random): Double
}
