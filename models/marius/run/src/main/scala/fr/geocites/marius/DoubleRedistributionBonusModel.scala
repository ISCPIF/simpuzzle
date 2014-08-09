package fr.geocites.marius

import fr.geocites.marius.balance.{NationalRedistribution, RegionalRedistribution, Bonus}
import fr.geocites.marius.state.FullNetworkState
import fr.geocites.marius.transaction.FixedCostTransaction

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


