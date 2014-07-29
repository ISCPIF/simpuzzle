/*
 * Copyright (C) 28/04/13 Romain Reuillon
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

package fr.geocites.simpoplocal

trait InnovationLife extends SimpopLocalStep {

  /// Length of time during which an innovation can be diffused from a settlement: passed this length of time this innovation becomes obsolete.
  def innovationLife: Int

  /**
   * Filters obsolete innovations from a set of innovations.
   * @param innovations The set innovations.
   * @param step The current step of the simulation.
   * @return The set of filtered innovations.
   */
  override def filterObsolete(innovations: Iterable[Innovation], step: Int) =
    innovations.filter(step - _.step <= innovationLife)

}
