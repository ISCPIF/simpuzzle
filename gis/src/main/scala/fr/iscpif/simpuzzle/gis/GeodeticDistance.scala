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

package fr.iscpif.simpuzzle

import java.awt.geom.Point2D

import fr.iscpif.simpuzzle.puzzle.Position
import org.geotools.referencing.GeodeticCalculator

package object gis {

  def geodedicDistance[T1: Position, T2: Position](p1: T1, p2: T2) = {
    val calc = new GeodeticCalculator

    val pos1 = implicitly[Position[T1]]
    val pos2 = implicitly[Position[T2]]

    val v1 = new Point2D.Double(pos1.x(p1), pos1.y(p1))
    val v2 = new Point2D.Double(pos2.x(p2), pos2.y(p2))
    calc.setStartingGeographicPoint(v1)
    calc.setDestinationGeographicPoint(v2)
    calc.getOrthodromicDistance
  }

}
