/*
 * Copyright (C) 2014 Romain Reuillon
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

package fr.geocites.marius.balance

import fr.geocites.marius.Marius

trait Redistribution <: Balances { model: Marius =>

  def territorialTaxes: Double
  def capitalShareOfTaxes: Double

  override def redistributionBalances(s: Seq[CITY]) = (regionalRedistributions(s) zip nationalRedistributions((s))).map { case (regional, national) => regional + national }

  def regionalRedistributions(s: Seq[CITY]): Seq[Double] = s.map(_ => 0.0)
  def nationalRedistributions(s: Seq[CITY]): Seq[Double] = s.map(_ => 0.0)

  def redistribution(s: Seq[CITY], territorialUnit: CITY => String, capital: CITY => Boolean) = {
    def deltas =
      for {
        (territory, indexedCities) <- s.zipWithIndex.groupBy { case (c, _) => territorialUnit(c) }
        (cities, indexes) = indexedCities.unzip
      } yield {
        val numberOfCapitals = cities.count(capital)
        val cityTaxes = cities.map(c => wealth.get(c) * territorialTaxes)

        val capitalShare =
          if(numberOfCapitals < 1) 0.0 else (capitalShareOfTaxes * cityTaxes.sum) / numberOfCapitals

        val taxesLeft = cityTaxes.sum - (capitalShare * numberOfCapitals)
        val territoryPopulation = cities.map(c => population.get(c)).sum

        val territorialDeltas = (cities zip cityTaxes).map {
          case (city, cityTaxes) =>
            val populationShare = population.get(city) / territoryPopulation

            val delta =
              (if (capital(city)) (taxesLeft * populationShare) + capitalShare
              else taxesLeft * populationShare) - cityTaxes
            delta
        }
        indexes zip territorialDeltas
      }

    deltas.flatten.toSeq.sortBy {
      case (i, _) => i
    }.unzip._2
  }

}
