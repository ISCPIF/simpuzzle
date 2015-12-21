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
package fr.iscpif.simpuzzle

import scalaz._

package object puzzle {

  def taggedMonad[M[_]: Monad, T]: Monad[λ[A => @@[M[A], T]]] = new Monad[λ[A => @@[M[A], T]]] {
    def innerMonad = implicitly[Monad[M[?]]]

    override def bind[A, B](fa: @@[M[A], T])(f: (A) => @@[M[B], T]): @@[M[B], T] = {
      val faw: M[A] = Tag.unwrap(fa)
      val fw: (A => M[B]) = (a: A) => Tag.unwrap(f(a))
      Tag(innerMonad.bind[A, B](faw)(fw))
    }

    override def point[A](a: => A): @@[M[A], T] = Tag(innerMonad.point(a))
  }

  trait Equivalence[T[_], M[_]] {
    def get[A](t: T[A]): M[A]
    def build[A](m: M[A]): T[A]
  }

  implicit def monadEquivalence[T[_], M[_]: Monad](implicit equivalence: Equivalence[T, M]) = new Monad[T] {
    def inner = implicitly[Monad[M]]
    override def bind[A, B](fa: T[A])(f: (A) => T[B]): T[B] =
      equivalence.build(inner.bind[A, B](equivalence.get(fa))(a => equivalence.get(f(a))))

    override def point[A](a: => A): T[A] =
      equivalence.build(inner.point(a))
  }

}
