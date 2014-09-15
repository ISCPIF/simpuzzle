package fr.iscpif.flocking.visualisation

import math._
import fr.iscpif.flocking.model.engine._
import fr.iscpif.flocking.model.datatypes._

object TestVisu extends App {

//  val model = new Model {
//    val worldWidth: Double = 32
//    val worldHeight: Double = 32
//    val envDivsHorizontal: Int = 32
//    val envDivsVertical: Int = 32
//    val populationSize: Int = 128
//    val vision: Double = 3
//    val visionObstacle: Double = 5/3.0
//    val minimumSeparation: Double = 1/3.0
//    val maxAlignTurn: Angle = Angle(toRadians(5))
//    val maxCohereTurn: Angle = Angle(toRadians(3))
//    val maxSeparateTurn: Angle = Angle(toRadians(1.5))
//    val stepSize: Double = 0.5
//  }
//
  val model = new Model {
    val worldWidth: Double = 32
    val worldHeight: Double = 32
    val envDivsHorizontal: Int = 32
    val envDivsVertical: Int = 32
    val populationSize: Int = 128
    val vision: Double = 1.172971493468721
    val visionObstacle: Double = 5/3.0
    val minimumSeparation: Double = 0.060163939217592576
    val maxAlignTurn: Angle = Angle(3.141592653589793)
    val maxCohereTurn: Angle = Angle(1.6847053731461783)
    val maxSeparateTurn: Angle = Angle(1.2964694250194273)
    val stepSize: Double = 0.5
  }



  val visu = new Visu {
        lazy val model: Model = TestVisu.model
        lazy val pixelWidth: Int = 500
        lazy val pixelHeight: Int = 500
        lazy val frameDelay: Int = 1000 / 24
        lazy val birdLength:Double = 0.02 * min(pixelWidth, pixelHeight)
        lazy val birdWidth: Double = 2
  } 

  // model.env.addDisc(model.worldWidth / 2.0, model.worldHeight / 2.0, 30.0, visu.obstacleColorRGB)
  //model.env.addDisc(0, 0, 30.0, visu.obstacleColorRGB)

  // val visu = new Visu with Fullscreen {
  //       lazy val model: Model = TestVisu.model
  //       lazy val frameDelay: Int = 1000 / 24
  //       lazy val birdLength:Double = 0.02 * min(pixelWidth, pixelHeight)
  //       lazy val birdWidth: Double = 2
  // } 
  visu.Skeleton.startup(null)
}
