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

package fr.geocites.marius.calibration

import fr.geocites.marius._

import scala.util.Random

object Evaluate extends App {

  implicit val rng = new Random(42)

  lazy val models = List(BonusFixedCostTest, ResourceBonusTest, DoubleRedistributionBonusTest, DoubleRedistributionResourceBonusTest)
  println(Console.YELLOW + "Choose you model: ")
  models.map(_.getClass.getName).zipWithIndex.foreach { case (c, i) => println(Console.GREEN + s"$i -> ${Console.GREEN} $c") }
  val i = io.StdIn.readInt()
  print(Console.RESET)

  for (
    j <- 0 until 10
  ) {
    val begin = System.currentTimeMillis()
    val evaluation = Evaluation.multiMacro(models(i))
    println("nb dead\tdistribution\toverflow")
    println(evaluation.map(_.formatted("%g")).mkString("\t"))
    println(System.currentTimeMillis() - begin)
  }

}
