package fr.geocites.marius

import fr.geocites.marius.balance.Bonus
import fr.geocites.marius.state.FullNetworkState
import fr.geocites.marius.transaction.FixedCostTransaction

/**
 * Created by clementinecottineau on 30/07/2014.
 */


class ResourceBonusModel(
    val economicMultiplier: Double,
    val sizeEffectOnSupply: Double,
    val sizeEffectOnDemand: Double,
    val distanceDecay: Double,
    val wealthToPopulationExponent: Double,
    val populationToWealthExponent: Double,
    val bonusMultiplier: Double,
    val fixedCost: Double,
    val oilAndGazEffect: Double,
    val coalEffect: Double) extends Marius with Bonus with FixedCostTransaction with SubSurfaceResources with DefaultValues with FullNetworkState
