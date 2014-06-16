/*
 * Copyright (C) 10/12/13 Romain Reuillon
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

package fr.geocites.marius.target

import util.Random

object Test extends App {
  //sizeEffectOnConsumption,sizeEffectOnProductivity,gamma,inversionPoint
  //127.1156496908556,38.621747226024965,63.49223984156123,11173.859183000295

  implicit val rng = new Random

  //  println(Calibration.fitness(
  //    sizeEffectOnProductivity = 0.118828737969146,
  //    sizeEffectOnConsumption = 0.00633657779460902,
  //    distanceDecay = 3.94794499219735,
  //    inversionPoint = 56.4235530249516,
  //    popMax = 20000,
  //    wMax = 10000))

  //0,0030635465	1,1006449254	176,8439810029

}
