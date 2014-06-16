package fr.geocites.marius.target

import scala.util.Random
import fr.geocite.simpuzzle.logging.NoLogging
import fr.geocite.marius._
import fr.geocite.marius.state._
import matching._

object Calibration {

  def fitness(model: Marius)(implicit rng: Random) = {
    evaluate(model, new TargetDistribution {})
  }

  def evaluate(model: Marius, target: Target)(implicit rng: Random): Double = {
    import model._
    val dynamic =
      model.states.map {
        _ match {
          case ValidState(s) => step.get(s) -> cities.get(s).map(population.get)
          case _ => return Double.PositiveInfinity
        }
      }
    target.target(dynamic)
  }

}
