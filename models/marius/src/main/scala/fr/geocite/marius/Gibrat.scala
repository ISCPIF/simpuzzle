/*
 * Copyright (C) 03/02/14 Romain Reuillon
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

package fr.geocite.marius

import fr.geocite.gibrat._
import fr.geocite.marius._

object Gibrat extends App {

  def popMin = 0

  val m = new Gibrat with MariusFile {
    override def stdRate = ???
    override def rate = ???

    def popMax = 18000
    def popMin = 0
    def wMin = 0
    def wMax = 40000
    def inversionPoint = 100

  }
}
