package fr.geocites.marius

import fr.geocites.marius.balance.{NationalRedistribution, Bonus}
import fr.geocites.marius.state.FullNetworkState
import fr.geocites.marius.transaction.FixedCostTransaction

/**
 * Created by clementinecottineau on 31/07/2014.
 */
class NationalRedistributionBonusModel (
                                 val economicMultiplier: Double,
                                 val sizeEffectOnSupply: Double,
                                 val sizeEffectOnDemand: Double,
                                 val distanceDecay: Double,
                                 val wealthToPopulationExponent: Double,
                                 val populationToWealthExponent: Double,
                                 val bonusMultiplier: Double,
                                 val fixedCost: Double,
                                 val territorialTaxes: Double,
                                 val capitalShareOfTaxes: Double) extends Marius with Bonus with FixedCostTransaction with NationalRedistribution with DefaultValues with FullNetworkState
