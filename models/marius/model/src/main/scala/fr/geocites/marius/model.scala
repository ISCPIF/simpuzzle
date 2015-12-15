/*
 * Copyright (C) 2015 Romain Reuillon
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
package fr.geocites.marius

import fr.geocites.marius.data._

import scalaz._
import Scalaz._

object model {

  case class Activity(sizeEffectOnDemand: Double, sizeEffectOnSupply: Double, exchangeRate: Double)

  case class MariusState(
    step: Int,
    cities: Vector[City],
    regions: Vector[Region],
    network: Network,
    distanceMatrix: DistanceMatrix)

  case class City(
    population: Double,
    wealth: Double,
    region: String,
    nation: String,
    regionalCapital: Boolean,
    nationalCapital: Boolean,
    oilOrGaz: Boolean,
    coal: Boolean)

  case class Region(id: String, urbanisationStep: Double)

  case class Interaction(from: Int, to: Int, transacted: Double)

  sealed trait Error
  case class AssertionFailed(msg: String) extends Exception(msg) with Error

  type Validate[A] = \/[Error, A]
  type Log[A] = Writer[Vector[Interaction], A]

  type LogValidate[A] = WriterT[Validate, Vector[Interaction], A]
  type StateLogValidate[A] = StateT[LogValidate, MariusState, A]
  type Step[A] = StateLogValidate[A]

  object Validate {
    implicit def apply[A](v: Validate[A]): LogValidate[A] =
      implicitly[MonadTrans[WriterT[?[_], Vector[Interaction], ?]]].liftM[Validate, A](v)
  }

  object LogValidate {
    implicit def apply[A](v: Log[A]): LogValidate[A] = WriterT[Validate, Vector[Interaction], A](v.run.right)
  }

  //object IsStep {

  implicit val logValidateIsStep = new IsStep[LogValidate] {
    override def toStep[A](s: LogValidate[A]): Step[A] = logValidateToStep(s)
  }

  implicit val stateIsStep = new IsStep[State[MariusState, ?]] {
    override def toStep[A](s: State[MariusState, A]): Step[A] = stateToStep(s)
  }

  implicit val stateTIsStep = new IsStep[StateT[Validate, MariusState, ?]] {
    override def toStep[A](s: StateT[Validate, MariusState, A]): Step[A] = stateTToStep(s)
  }

  implicit def logValidateToStep[A](v: LogValidate[A]): Step[A] =
    implicitly[MonadTrans[StateT[?[_], MariusState, ?]]].liftM[LogValidate, A](v)

  implicit def validateToStep[A](v: Validate[A]): Step[A] = Validate(v)

  implicit def logToStep[A](v: Log[A]): Step[A] = LogValidate(v)

  implicit def stateToStep[A](v: State[MariusState, A]): Step[A] = StateT[LogValidate, MariusState, A] {
    s => v.run(s).point[LogValidate]
  }

  implicit def stateTToStep[A](v: StateT[Validate, MariusState, A]): Step[A] = v
  // }

  trait IsStep[S[_]] {
    def toStep[A](s: S[A]): Step[A]
  }

  implicit class IsStepDecorator[S[_]: IsStep, A](s: S[A]) {
    def toStep: Step[A] = implicitly[IsStep[S]].toStep[A](s)
  }

  //type StateTReaderTOption[C, S, A] = StateT[({ type l[X] = ReaderTOption[C, X] })#l, S, A]
  /*object WriterTValidate extends KleisliInstances with KleisliFunctions {
    def apply[A, B](f: A => Validate[B]): WriterTValidate[A, B] = f
  }*/

  /* type StateTReaderTOption[C, S, A] = StateT[({ type l[X] = ReaderTOption[C, X] })#l, S, A]

  object StateTReaderTOption extends StateTInstances with StateTFunctions {
    def apply[C, S, A](f: S => (S, A)) = new StateT[({ type l[X] = ReaderTOption[C, X] })#l, S, A] {
      def apply(s: S) = f(s).point[({ type l[X] = ReaderTOption[C, X] })#l]
    }
    def get[C, S]: StateTReaderTOption[C, S, S] =
      StateTReaderTOption { s => (s, s) }
    def put[C, S](s: S): StateTReaderTOption[C, S, Unit] =
      StateTReaderTOption { _ => (s, ()) }
  }*/

  object Logger {
    def empty = new Logger {
      override def apply[A](a: A, interactions: Vector[Interaction]): Log[A] = a.point[Log]
    }
  }

  trait Logger {
    def apply[A](a: A, interactions: Vector[Interaction] = Vector.empty): Log[A]
  }

  /*def nextState(s: STATE)(implicit rng: Random): Log[STATE] = {
    for {
      newWealths <- wealths(s)
    } yield {
      def newPopulations =
        (cities.get(s) zip newWealths).zipWithIndex.map {
          case ((city, newWealth), i) =>
            check(newWealth >= 0, s"City $i error in wealth before conversion toPop $newWealth")
            val deltaPopulation = (wealthToPopulation(newWealth) - wealthToPopulation(wealth.get(city))) / economicMultiplier
            val newPopulation = population.get(city) + deltaPopulation
            check(newPopulation >= 0, s"Error in population $newWealth $newPopulation")
            newPopulation
        }

      def newCities =
        (cities.get(s) zip newPopulations zip newWealths).map(flatten).map {
          case (city, newPopulation, newWealth) =>
            check(newPopulation >= 0, s"The population of $city is negative $newPopulation, $newWealth")
            population.set(newPopulation)(wealth.set(newWealth)(city))
        }

      def updatedState = urbanTransition(cities.set(newCities)(s))
      step.modify(_ + 1)(updatedState)
    }
  }*/

  def marius(
    transactions: Transactions,
    interactionPotential: InteractionPotential,
    balance: Balance,
    economicMultiplier: Double,
    wealthToPopulationExponent: Double,
    activities: Vector[Activity])(implicit log: Logger = Logger.empty): Step[Vector[City]] = {

    def wealthToPopulation(wealth: Double) =
      if (wealth >= 0) math.pow(wealth, wealthToPopulationExponent).right
      else AssertionFailed(s"Negative wealth $wealth").left

    def populations(newWealths: Vector[Double]): ReaderT[Validate, MariusState, Vector[Double]] = ReaderT { state: MariusState =>
      (state.cities zip newWealths).zipWithIndex.traverse { case ((city, newWealth), i) => population(city, newWealth, i) }
    }

    def population(city: City, newWealth: Double, index: Int): Validate[Double] =
      for {
        _ <- if (newWealth < 0) AssertionFailed(s"City $index error in wealth before conversion toPop $newWealth").left else Unit.right
        newPop <- wealthToPopulation(newWealth)
        oldPop <- wealthToPopulation(city.wealth)
        deltaPopulation = (newPop - oldPop) / economicMultiplier
        newPopulation = city.population + deltaPopulation
        _ <- if (newPopulation < 0) AssertionFailed(s"Population is negative for city $index: $newPopulation").left else Unit.right
      } yield newPopulation

    def newState(newPopulations: Vector[Double], newWealths: Vector[Double]) = State { state: MariusState =>
      val newCities = (state.cities zip newPopulations zip newWealths).map {
        case ((city, newPopulation), newWealth) => city.copy(population = newPopulation, wealth = newWealth)
      }
      val newState = state.copy(step = state.step + 1, cities = newCities)
      (newState, newCities)
    }

    for {
      newWealths <- wealth(transactions, interactionPotential, balance, economicMultiplier, activities).state
      newPopulations <- stateTToStep(populations(newWealths).state)
      newCities <- stateToStep(newState(newPopulations, newWealths))
    } yield newCities

  }

  def wealth(
    transactions: Transactions,
    potential: InteractionPotential,
    balance: Balance,
    economicMultiplier: Double,
    activities: Vector[Activity])(implicit log: Logger) = ReaderT[LogValidate, MariusState, Vector[Double]] { state =>
    def populations = state.cities.map(_.population)

    def delta(activity: Activity) = {
      val suppliesOfCities = supplies(populations, economicMultiplier, activity)
      val demandsOfCities = demands(populations, economicMultiplier, activity)
      LogValidate(deltaWealth(transactions, potential, balance)(state.distanceMatrix, state.network, state.cities, suppliesOfCities, demandsOfCities))
    }

    for {
      a <- activities.traverse[LogValidate, Vector[Double]](delta)
    } yield a.transpose.map(_.sum)
  }

  def deltaWealth(
    transactions: Transactions,
    potential: InteractionPotential,
    balance: Balance)(
      distances: DistanceMatrix,
      network: Network,
      cities: Vector[City],
      supplies: Vector[Double],
      demands: Vector[Double])(implicit log: Logger) = {
    def populations = cities.map(_.population)

    val t =
      Transacted(
        cities,
        supplies,
        demands,
        transactions(distances, network, supplies, demands, potential)
      )

    def interactions =
      for {
        (row, i) <- t.transacted.lines.zipWithIndex
        Cell(j, value) <- row
      } yield Interaction(i, j, value)

    (balance(t) zip supplies zip demands) map {
      case ((balance, supply), demand) => supply - demand + balance
    }

    log(balance(t), interactions.toVector)
  }

  case class Transacted(cities: Vector[City], supplies: Vector[Double], demands: Vector[Double], transacted: Matrix) {
    lazy val transposedTransacted = transacted.transpose
    lazy val transactedFromSum = transacted.linesContent.map(_.sum)
    lazy val transactedToSum = transposedTransacted.linesContent.map(_.sum)
  }

  type Balance = Reader[Transacted, Vector[Double]]

  implicit class BalanceOperations(b1: Balance) {
    def +(b2: Balance) =
      for {
        l <- b1
        r <- b2
      } yield (l zip r) map Function.tupled(_ + _)

    def -(b2: Balance) =
      for {
        l <- b1
        r <- b2
      } yield (l zip r) map Function.tupled(_ - _)
  }

  def unsolds: Balance = Reader { t => for { (supply, i) <- t.supplies.zipWithIndex } yield supply - t.transactedFromSum(i) }
  def unsatisfieds: Balance = Reader { t => for { (demand, i) <- t.demands.zipWithIndex } yield demand - t.transactedToSum(i) }

  def bonusFixedCostsBalance(fixedCost: Double, bonusMultiplier: Double): Balance = Reader { t =>
    def fixedCosts: Seq[Double] =
      t.transacted.linesContent.map { _.count(_ > 0.0) * fixedCost }

    def bonuses: Vector[Double] = {
      def diversityBonuses = {
        def transactedWith(transacted: Vector[Cell]) =
          transacted.filter { case Cell(_, v) => v > 0 }.map { case Cell(to, _) => to }

        (t.transacted.lines zip t.transposedTransacted.lines) map {
          case (from, to) => (transactedWith(from.toVector).toSet union transactedWith(to.toVector).toSet).size / t.cities.size.toDouble
        }
      }

      def importVolumes: Vector[Double] =
        for { (demand, i) <- t.demands.zipWithIndex } yield t.transactedToSum(i)

      def exportVolumes: Vector[Double] =
        for { (supply, i) <- t.supplies.zipWithIndex } yield t.transactedFromSum(i)

      (importVolumes zip exportVolumes zip diversityBonuses).map {
        case ((importVolume, exportVolume), diversityBonus) =>
          bonusMultiplier * (importVolume + exportVolume) * diversityBonus
      }
    }

    (bonuses zip fixedCosts).map { case (bonus, fixedCosts) => bonus - fixedCosts }
  }

  type Transactions = (DistanceMatrix, Network, Vector[Double], Vector[Double], InteractionPotential) => Matrix

  /** Filter the interaction potential matrix */
  def fixedCostTransactions(fixedCost: Double): Transactions = (
    distances: DistanceMatrix,
    network: Network,
    supplies: Vector[Double],
    demands: Vector[Double],
    potential: InteractionPotential) => {

    def interactionPotentials(distances: DistanceMatrix, network: Network, supplies: Vector[Double], demands: Vector[Double], potential: InteractionPotential) = {
      val iM1 = supplies.toArray
      val iM2 = demands.toArray
      network.mapNodes { (i, j) => potential(iM1(i), iM2(j), distances(i)(j)) }
    }

    val interactionMatrixValue = interactionPotentials(distances, network, supplies, demands, potential)
    val fromInteractionPotentialSum = interactionMatrixValue.transpose.linesContent.map(_.sum)

    interactionMatrixValue.map {
      (from, to, interactionPotential) =>
        if (interactionPotential > 0) {
          val fSupply = supplies(from)
          val fromIPSum = fromInteractionPotentialSum(from)
          val normalisedIPFrom = interactionPotential / fromIPSum
          if (normalisedIPFrom * fSupply > fixedCost) interactionPotential else 0.0
        } else 0.0
    }
  }

  type InteractionPotential = (Double, Double, Double) => Double

  def gravityPotential(distanceDecay: Double): InteractionPotential =
    (mass1: Double, mass2: Double, distance: Double) => (mass1 * mass2) / math.pow(distance, distanceDecay)

  def supplies(populations: Vector[Double], economicMultiplier: Double, activity: Activity) =
    populations.map(p => supply(p, economicMultiplier, activity))

  def demands(populations: Vector[Double], economicMultiplier: Double, activity: Activity) =
    populations.map(p => demand(p, economicMultiplier, activity))

  def demand(population: Double, economicMultiplier: Double, activity: Activity) =
    economicMultiplier * activity.exchangeRate * math.pow(population, activity.sizeEffectOnDemand)

  def supply(population: Double, economicMultiplier: Double, activity: Activity) =
    economicMultiplier * activity.exchangeRate * math.pow(population, activity.sizeEffectOnSupply)

}
