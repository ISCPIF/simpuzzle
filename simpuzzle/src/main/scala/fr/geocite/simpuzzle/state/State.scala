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

package fr.geocite.simpuzzle.state

import scala.reflect.macros._
import scala.reflect.runtime.universe._
import scala.reflect._
import scala.reflect.api._
import scala.language.experimental.macros

object State {
  def ct[S](c: Context): c.Expr[ClassTag[S]] = {
    import c.universe._
    val a = c.prefix.tree.tpe.member(newTypeName("VALID_STATE")).typeSignature
    c.Expr(q"implicitly[ClassTag[$a]]").asInstanceOf[c.Expr[ClassTag[S]]]
  }
}

trait State {
  type VALID_STATE <: STATE
  type STATE

  implicit def validStateTag: reflect.ClassTag[VALID_STATE] = macro State.ct[VALID_STATE]
}
