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

package fr.geocites.marius

import fr.geocites.gugus.redistribution.{ TerritorialTaxes, Redistribution }
import monocle.SimpleLens

trait RegionalRedistribution <: Redistribution with TerritorialTaxes {
  def region: SimpleLens[CITY, String]
  def regionalCapital: SimpleLens[CITY, Boolean]
  def regionalRedistributions(s: Seq[CITY]): Seq[Double] = redistribution(s, region.get _, regionalCapital.get _)
}
