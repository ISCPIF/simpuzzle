/*
 * Copyright (C) 20/05/2014 Guillaume Chérel
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

import fr.iscpif.flocking.model.engine._
import fr.iscpif.flocking.model.interactions._
import fr.iscpif.flocking.model.birds._
import fr.iscpif.flocking.model.datatypes._
import scala.annotation.tailrec
import scala.util.Random
import scala.math._


class BehaviourComputing {
  def compute(
        _worldWidth: Double,
        _worldHeight: Double,
        _populationSize: Int,
        _stepSize: Double,
        _vision: Double,
        _minimumSeparation: Double,
        _maxAlignTurn: Double,
        _maxCohereTurn: Double,
        _maxSeparateTurn: Double
               )(implicit rng: Random): Array[Double] = {
    new Behaviour {
      val model = new Model {
        val worldWidth: Double = _worldWidth //32
        val worldHeight: Double = _worldHeight //32
        val populationSize: Int = _populationSize
        val vision: Double = _vision
        val minimumSeparation: Double = _minimumSeparation
        val maxAlignTurn: Angle = Angle(_maxAlignTurn)
        val maxCohereTurn: Angle = Angle(_maxCohereTurn)
        val maxSeparateTurn: Angle = Angle(_maxSeparateTurn)
        val stepSize: Double = _stepSize // 0.05
        val envDivsHorizontal: Int = 1
        val envDivsVertical: Int = 1
        val visionObstacle: Double = 1
      }
    }.defaultDescription.toArray
  }
}

trait Behaviour {
    // type NGroups = Int
    // type AvgVelocity = Double
    // type RelativeDiffusion = Double
    // type B = ([NGroups], [AvgVelocity], [RelativeDiffusion])

    val model: Model

    def countGroups(gb: GraphBirds): Int = countGroups(gb, 0, (0 until gb.birds.size).toSet)
    def countGroups(gb: GraphBirds, nclustersFound: Int, remaining: Set[Int]): Int = {
      if (remaining.size == 0) nclustersFound
      else countGroups(gb, nclustersFound + 1, remaining -- extractComponent(gb, remaining.head, Set()))
    }

    def extractComponent(gb: GraphBirds, start: Int, visited: Set[Int]): Set[Int] = {
      if (gb.birds.size == 0) Set()
      else {
        val neighbours: Seq[Int] = gb.flockmates(start)
        if (neighbours.size == 0) Set(start)
        else neighbours.foldLeft(visited + start)((a:Set[Int], b:Int) => if (!a.contains(b)) extractComponent(gb, b, a) else a)
      }
    }

    def nearestNeighbour(d: DistMatrix)(i: Int, birds: Seq[Bird]): Int = {
      birds.indices.minBy(j => if (i != j) d(i, j) else Double.MaxValue)
    }

    def voronoiNeighbours(birds: Seq[Bird], dm: DistMatrix): Seq[Seq[Int]] = {
      val nnf = nearestNeighbour(dm)_
      val nn = for {i <- birds.indices} yield nnf(i, birds)
      for {i <- birds.indices} yield voronoiNeighbours(birds, nn, i)
    }
    def voronoiNeighbours(birds: Seq[Bird], nearestNeigh: Seq[Int], i: Int): Seq[Int] =
      for {j <- birds.indices if ((i != j) && nearestNeigh(j) == i)} yield j

    def kNearestNeighbours(k: Int, birds:Seq[Bird], dm: DistMatrix): Seq[Seq[Int]] = {
      def insert(x: Int, k: Int, nn: List[Int], distFromI: Int => Double): List[Int] =
        if (k == 0) List()
        else if (nn.size == 0) List(x)
        else if (distFromI(x) < distFromI(nn.head)) (x :: nn) take k
        else nn.head :: insert(x, k - 1, nn.tail, distFromI)
      
      def knn(i: Int): Seq[Int] = 
        birds.indices.foldRight(List[Int]())((j,nn) => if (j == i) nn else insert(j, k, nn, {dm(i,_)}))
      
      birds.indices.map(knn(_))
    }

    def distBetween(neighbours: Seq[Seq[Int]], dm: DistMatrix): Seq[Seq[Double]] =
      neighbours.indices.map((i: Int) => neighbours(i).map((j: Int) => dm(i,j)))

    def sumOver(is: Range, f: Int => Double): Double = (is map f).sum
    def averageOver(is: Range, f: Int => Double): Double =
      sumOver(is, f) / (is.size: Double)

    def relativeDiffusion(neighboursDistAtT1: Seq[Seq[Double]],
      neighboursDistAtT2: Seq[Seq[Double]]): Double = {
      averageOver(neighboursDistAtT1.indices, {i => {
        val ni: Double = neighboursDistAtT1(i).size
        (1 / ni) * sumOver(neighboursDistAtT1(i).indices, {j =>
          1 - (pow(neighboursDistAtT1(i)(j), 2) / pow(neighboursDistAtT2(i)(j), 2))
          })
      }
      })
    }

    abstract class AbstractCollector[S, +T]
    case class Collector[S, +T](when: Int, f: S => AbstractCollector[S,T]) extends AbstractCollector[S, T] {
      def collect(modelstate: S): AbstractCollector[S, T] = f(modelstate)
    }
    case class Val[S,+T](f: T) extends AbstractCollector[S, T]

    def collectCountGroups(state: GraphBirds): Double =
      countGroups(state) / (model.populationSize: Double)
    val countGroupsCollector: Collector[GraphBirds, Double] =
      Collector(1000, { (s: GraphBirds) => Val(collectCountGroups(s)) })

    def collectRelativeDiffusion(state1: GraphBirds)(state2: GraphBirds): Double = {
      val dm = DistMatrix(state1.birds.map(_.position), model.distanceBetween)
      val neighbs = kNearestNeighbours(3,state1.birds, dm)
      val dist1 = distBetween(neighbs, dm)
      relativeDiffusion(dist1, distBetween(neighbs, DistMatrix(state2.birds.map(_.position), model.distanceBetween)))
    }
    val relativeDiffusionCollector: Collector[GraphBirds, Double] =
      Collector(950, { (s1:GraphBirds) =>
        Collector(1000, { (s2: GraphBirds) => Val(collectRelativeDiffusion(s1)(s2))})
        })

    def collectVelocity(state1: GraphBirds)(state2: GraphBirds): Double =
      (state1.birds zip state2.birds).map(x => model.distanceBetween(x._1.position, x._2.position) / 100.0).sum / (state1.birds.size: Double) 
    val velocityCollector: Collector[GraphBirds, Double] =
      Collector(600, { (s1:GraphBirds) =>
      Collector(1000, { (s2:GraphBirds) =>  Val(
           collectVelocity(s1)(s2)
         ) })})
//    val velocityCollector: Collector[GraphBirds, Double] =
//      Collector(500, { (s0:GraphBirds) =>
//      Collector(600, { (s1:GraphBirds) =>
//      Collector(700, { (s2:GraphBirds) =>
//      Collector(800, { (s3:GraphBirds) =>
//      Collector(900, { (s4:GraphBirds) =>
//      Collector(1000, { (s5:GraphBirds) =>  Val(
//           (collectVelocity(s0)(s1) + collectVelocity(s1)(s2) + collectVelocity(s2)(s3) +collectVelocity(s3)(s4) +collectVelocity(s4)(s5)) / 5.0 
//         ) })})})})})})

    @tailrec final def constructDescription(gb: GraphBirds, iter: Int, collectors: AbstractCollector[GraphBirds, Double]*): Seq[Double] =
      if (collectors.exists(x => x match {case Collector(_,_) => true
        case Val(_) => false })) {
        val updatedCollectors: Seq[AbstractCollector[GraphBirds, Double]] = collectors.map(x => x match {case Collector(i,f) => if (i == iter) f(gb) else x
         case Val(_) => x})
        val updatedState = model.oneStep(gb)
        constructDescription(updatedState, iter + 1, updatedCollectors: _*)
      }
      else collectors.map(_ match { case Val(x) => x } )

    def defaultDescription(implicit rng: Random) = constructDescription(model.randomInit, 0, countGroupsCollector, relativeDiffusionCollector, velocityCollector)

    trait DistMatrix {
      val distances: Vector[Vector[Double]]
      def apply(i: Int,j: Int): Double =
      if (i == j) 0
      else if (i < j) distances(i)(j - i - 1)
      else apply(j,i)
    }
    object DistMatrix {
      def apply(points: Seq[Point], distFunc: (Point, Point) => Double): DistMatrix = new DistMatrix {
        val distances: Vector[Vector[Double]] = (for {i <- 0 until (points.size - 1)} yield (for {j <- i+1 until points.size} yield distFunc(points(i), points(j))).toVector).toVector
      }
      def euclidean(p1: Point, p2: Point): Double = sqrt(pow(p1.x - p2.x, 2) + pow(p1.y - p2.y,2))
    }

}

