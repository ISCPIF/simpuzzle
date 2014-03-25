/*
 * Copyright (C) 16/09/13 Romain Reuillon
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

package fr.geocite.simpoplocal

import fr.geocite.simpuzzle.logging.Logging

trait SimpopLocalLogging <: Logging {

  type LOGGING = LogInfo

  sealed trait LogInfo
  case class Diffused(from: Int, to: Int, rootId: Int) extends LogInfo
  case class Created(in: Int, rootId: Int) extends LogInfo
}
