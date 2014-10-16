package fr.geocites.marius

import fr.geocites.marius.balance._
import fr.geocites.marius.state._
import fr.geocites.gugus.transaction._
import fr.geocites.gugus.balance._

/**
 * Created by clementinecottineau on 08/08/2014.
 */
class DoubleRedistributionBonusModel (
                                       val economicMultiplier: Double,
                                       val sizeEffectOnSupply: Double,
                                       val sizeEffectOnDemand: Double,
                                       val distanceDecay: Double,
                                       val wealthToPopulationExponent: Double,
                                       val populationToWealthExponent: Double,
                                       val bonusMultiplier: Double,
                                       val fixedCost: Double,
                                       val territorialTaxes: Double,
                                       val capitalShareOfTaxes: Double)

  extends Marius with Bonus with FixedCostTransaction with RegionalRedistribution with NationalRedistribution with DefaultValues with FullNetworkState


