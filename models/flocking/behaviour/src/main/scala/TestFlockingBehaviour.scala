/*
 * Copyright (C) Guillaume Chérel 11/09/14
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


package fr.iscpif.flocking.behaviour

import util.Random
//import scala.math._

import scala.util.Random
//import java.io.File
//import java.io.PrintWriter

object TestFlockingBehaviour extends App {

implicit val rng = new Random

println((new BehaviourComputing).compute(
        _worldWidth = 32,
        _worldHeight = 32,
        _populationSize = 128,
        _stepSize = 0.5,
        _vision = 3,
        _minimumSeparation = 1/3.0,
        _maxAlignTurn = 0.09, //5 degres
        _maxCohereTurn = 0.05, //3 degres
        _maxSeparateTurn = 0.03).mkString(", "))//1.5 degres

}


//
//object FlockingBehaviourSearch extends App {
//
//  implicit val rng = new Random
//
//  val m = new BehaviourSearch {
//
//    override def cloneProbability: Double = 0.1
//
//    /* number of nearest neighbours to compute a simulations novelty score */
//    def k = 5
//
//    override def genomeSize: Int = 6
//
//    def min = Vector[Double](2,0,0,0,0,0)
//    def max = Vector[Double](500,1,1,Pi,Pi,Pi)
//
//    /** Number of steps before the algorithm stops */
//    override def steps = 100000
//
//    /** the size of the offspring */
//    override def lambda = 2
//
//    override type P = Seq[Double]
//    override def express(g: G, rng: Random): P = {
//      Behaviour(g.values(0).toInt,
//                g.values(1),
//                g.values(2),
//                g.values(3),
//                g.values(4),
//                g.values(5))
//    }
//
//    override type Niche = Seq[Int]
//    override val keepN = 1
//    val divsSizes: Seq[Double] = Vector[Double](0.1, 0.01, 0.1)
//    override def niche(individual: Individual[G, P, F]): Niche =
//      (individual.phenotype zip divsSizes) map {case (x, d) => (x / d).toInt} toSeq
//  }
//
//  val phenotypeSize = 3
//
//  val f = new File(s"/tmp/flockingbse").mkdir()
//
//
//  m.evolve.untilConverged {
//    s =>
//      val f = new File(s"/tmp/flockingbse/population${s.generation}.csv")
//      if (f.exists) f.delete
//      val output = new PrintWriter(f)
//      output.write((0 until m.genomeSize).map("par" + _).mkString(",") + "," + (0 until phenotypeSize).map("bhv" + _).mkString(",") + ",knndistance," + (0 until phenotypeSize).map("niche"+_).mkString(",") + "\n")
//      val diversities = m.diversity(s.population.map(i => m.doubleSeq.get(i.phenotype)))
//      (s.population.content zip diversities).foreach { case (i, div) => {
//        output.write(m.scale(i.genome).values.mkString(",") + "," + i.phenotype.mkString(",") + "," + div + "," + m.niche(i.toIndividual).mkString(",") + "\n")
//      }}
//      //if (s.generation > 1) new File(s"./population${s.generation - 1}.csv").delete()
//      println("step " + s.generation + " popsize " + s.population.content.size + " volume discovered " + s.population.toIndividuals.groupBy(m.niche).size)
//      output.close
//  }
//
//}
