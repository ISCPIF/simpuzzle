package fr.geocites.marius.target

import scala.util.Random
import fr.geocite.simpuzzle.logging.NoLogging
import fr.geocite.marius._
import fr.geocite.marius.state._
import matching._

object Calibration {

  def fitness(marius: Marius with MariusFile with MariusCity)(implicit rng: Random) =
    (new TargetDistribution {}).distribution(marius).toArray

}
