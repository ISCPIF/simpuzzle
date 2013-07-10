/*
 * Copyright (C) 04/07/13 Romain Reuillon
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

package fr.geocite.gis.distance

import org.geotools.referencing.GeodeticCalculator
import fr.geocite.simpuzzle.distance.GeometricDistance
import fr.geocite.simpuzzle.city.Position
import java.awt.geom.Point2D

trait GeodeticDistance <: GeometricDistance {

  def distance(p1: Position, p2: Position) = {
    lazy val calc = new GeodeticCalculator

    val v1 = new Point2D.Double(p1.x, p1.y)
    val v2 = new Point2D.Double(p2.x, p2.y)
    calc.setStartingGeographicPoint(v1)
    calc.setDestinationGeographicPoint(v2)
    calc.getOrthodromicDistance
  }

}
