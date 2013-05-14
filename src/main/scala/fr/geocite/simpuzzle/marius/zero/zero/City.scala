package fr.geocite.simpuzzle.marius.zero.zero

import scala.util.Random
import scala.math._

object City {
  def apply(popMin: Int, popMax: Int)(implicit rng: Random) = {
    val population = (round(popMin + rng.nextDouble * (popMax - popMin)))
    val xcor = round(rng.nextDouble() * 600).toInt
    val ycor = round(rng.nextDouble() * 600).toInt
    new City(population, xcor, ycor)
  }
}

case class City(population: Double, x: Int, y: Int)
