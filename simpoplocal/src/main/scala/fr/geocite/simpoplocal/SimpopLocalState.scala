/*
 * Copyright (C) 25/04/13 Romain Reuillon
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

import util.Random
import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.tailrec
import fr.geocite.simpuzzle.city.{ Id, Radius, Position }

trait SimpopLocalState extends fr.geocite.simpuzzle.State {
  case class SimpopLocalState(date: Int, cities: Seq[City]) {
    def step = date
  }

  case class City(
      id: Int,
      x: Double,
      y: Double,
      population: Double,
      availableResource: Double,
      //resourceMax: Double,
      percolationIndex: Int,
      cityClass: Int,
      tradePlace: TradePlace) extends Position with Radius with Id {

    def rangeRadiusClass1 = 20.0
    def rangeRadiusClass2 = 10.0
    def rangeRadiusClass3 = 5.0

    def radius =
      cityClass match {
        case 1 => rangeRadiusClass1
        case 2 => rangeRadiusClass2
        case 3 => rangeRadiusClass3
        case _ => error(s"Invalid city class $cityClass")
      }
  }

  object TradePlace {

    def apply(
      innovations: List[Innovation] = List.empty,
      countCreatedInnovation: Int = 0,
      countAcquiredInnovation: Int = 0) = new TradePlace(innovations.sorted(Innovation.orderByRootId), countCreatedInnovation, countAcquiredInnovation)
  }

  /**
   *
   * @param innovations
   * @param countCreatedInnovation
   * @param countAcquiredInnovation
   */
  class TradePlace private (
      val innovations: List[Innovation] = List.empty,
      val countCreatedInnovation: Int = 0,
      val countAcquiredInnovation: Int = 0) {

    def copy(
      innovations: List[Innovation] = this.innovations,
      countCreatedInnovation: Int = this.countCreatedInnovation,
      countAcquiredInnovation: Int = this.countAcquiredInnovation) = TradePlace(innovations, countCreatedInnovation, countAcquiredInnovation)

    /** Return the total innovation in the trade place : adopt + created **/
    def totalInnovation = countCreatedInnovation + countAcquiredInnovation

    /**
     * Dyssimetric diff between this two pool of innovation (mine and the list in the param), we want to remove all equal fatherInnovation
     * @param innovationsAvailable
     * @return
     */
    def filterInnovations(innovationsAvailable: List[Innovation]) =
      diff(innovationsAvailable, innovations).toIndexedSeq

    /**
     * Create and register zero innovation by turn
     * @param city
     * @param time
     * @return
     */
    def registerOriginalInnovation(
      city: Int,
      time: Int) = {

      val innovation = new Innovation(city = city, date = time)

      val newTradePlace =
        this.copy(
          innovation :: innovations,
          countCreatedInnovation + 1,
          countAcquiredInnovation
        )

      val newHistory = List(ExchangeLine(time, city, city, innovation))
      (newTradePlace, newHistory)
    }

    /**
     * Duplicate and Register multiple copy of innovation
     * @param newInnovations
     * @param city
     * @param time
     * @return
     */
    def registerCopyOfInnovations(
      newInnovations: Iterable[Innovation],
      city: City,
      time: Int) = {

      // Need to recopy innovation object to store hierarchical diffusion
      // ( before, after )
      val copyOfInnovation =
        newInnovations.toList.map {
          innovation =>
            (innovation, innovation.cloneWith(city = city.id, date = time))
        }

      val newHistory =
        copyOfInnovation.map {
          case (before, after) => ExchangeLine(time, before.city, after.city, after)
        }

      val newTradePlace =
        this.copy(
          copyOfInnovation.map {
            case (before, after) => after
          } ::: this.innovations,
          countCreatedInnovation,
          countAcquiredInnovation + newInnovations.size
        )

      (newTradePlace, newHistory)
    }

    /**
     *
     * @param popCityStart
     * @param popCityEnd
     * @param distance
     * @param distanceF
     * @param pSuccessAdoption
     * @param aprng
     * @return
     */
    def computeInteractionInterCities(popCityStart: Double,
      popCityEnd: Double,
      distance: Double,
      distanceF: Double,
      pSuccessAdoption: Double)(implicit aprng: Random) = {

      val population = (popCityStart * popCityEnd)
      val formula = (population / math.pow(distance, distanceF))
      val pCopyInnovation = aprng.nextDouble

      val pBinomial = binomial(formula, pSuccessAdoption)

      pCopyInnovation <= pBinomial
    }

    /**
     *
     * @param popCity
     * @param pSuccessInteraction
     * @param aprng
     * @return
     */
    def computeInteractionIntraCities(popCity: Double,
      pSuccessInteraction: Double)(implicit aprng: Random): Boolean = {

      val formula = ((1.0 / 2.0) * (popCity * (popCity - 1.0)))
      val pCreateInnovation = aprng.nextDouble

      val pBinomial = binomial(formula, pSuccessInteraction)

      pCreateInnovation <= pBinomial
    }

  }

  def binomial(pool: Double, p: Double): Double = (1.0 - (math.pow((1 - p), pool)))

  implicit def indexedSeq2IndexedSeqDecorator[T](elts: IndexedSeq[T]) = new {
    def randomElement(implicit prng: Random) = if (elts.isEmpty) None else Some(elts(prng.nextInt(elts.size)))
  }

  def diffNonSorted[A](l: List[A], r: List[A])(implicit order: Ordering[A]) =
    diff(l.sorted(order), r.sorted(order))

  @tailrec final def diff[A](l: List[A], r: List[A], acc: List[A] = List.empty)(implicit order: Ordering[A]): List[A] = {
    (l.headOption, r.headOption) match {
      case (None, None) => acc.reverse
      case (_, None) => l ::: (acc.reverse)
      case (None, _) => acc.reverse
      case (Some(el), Some(er)) =>
        if (order.equiv(el, er)) diff(l.tail, r, acc)
        else if (order.lt(el, er)) diff(l.tail, r, el :: acc)
        else diff(l, r.tail, acc)
    }
  }

  def dist(x: Double,
    y: Double,
    xOut: Double,
    yOut: Double): Double = math.sqrt(math.pow((x - xOut), 2) + math.pow((y - yOut), 2))

  object Innovation {
    implicit val orderByRootId = Ordering.by((_: Innovation).rootId)
    val curId = new AtomicInteger
  }

  class Innovation(
      val city: Int,
      val date: Int,
      _rootId: Option[Int] = None,
      val id: Int = Innovation.curId.getAndIncrement) {

    val rootId = _rootId.getOrElse(id)

    def cloneWith(
      city: Int = this.city,
      date: Int = this.date) = {

      new Innovation(city,
        date,
        Some(rootId)
      )
    }

    override def toString = "Id=" + id + ", RootId=" + rootId
  }

  case class ExchangeLine(date: Int, cityStart: Int, cityEnd: Int, innovation: Innovation)

  case class CityDist(cityId: Int, distance: Double)

  type STATE = SimpopLocalState
}
