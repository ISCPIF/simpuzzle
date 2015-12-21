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
package fr.iscpif.simpuzzle.puzzle

import scalaz.Scalaz._
import scalaz._

object Puzzle {
  def apply[S, L, E] = new Puzzle[S, L, E] {}
}

trait Puzzle[S, L, E] {

  object Log {
    implicit def logMonadEquivalence = new Equivalence[Log, Writer[Vector[L], ?]] {
      override def get[A](t: Log[A]): Writer[Vector[L], A] = t.writer

      override def build[A](m: Writer[Vector[L], A]): Log[A] = Log(m)
    }

    implicit class LogDecorator[A](l: Log[A]) {
      def toValidate: LogValidate[A] = LogValidate(WriterT[Validate, Vector[L], A] {
        l.writer.run.point[Validate]
      })

      def toStep: Step[A] = l.toValidate
    }

    def log[A](a: A): Log[A] = a.point[Log]

    implicit val loggerMonad = monadEquivalence[Log, Writer[Vector[L], ?]]
  }

  case class Log[A](writer: Writer[Vector[L], A])

  object Logger {
    def empty = new Logger {
      override def apply[A](a: A, interactions: Vector[L]): Log[A] = Log.log(a)
    }
  }

  trait Logger {
    def apply[A](a: A, interactions: Vector[L] = Vector.empty): Log[A]
  }

  object Validate {
    implicit def validateMonadEquivalence = new Equivalence[Validate, \/[E, ?]] {
      override def get[A](t: Validate[A]) = t.v

      override def build[A](m: \/[E, A]) = Validate(m)
    }

    implicit def validateToStep[A](v: Validate[A]): Step[A] = LogValidate.logValidate(v)

    implicit class ValidateDecorator[A](v: Validate[A]) {
      def toStep: Step[A] = v
    }

    implicit def vToValidate[A](v: \/[E, A]) = Validate(v)

    implicit val validateMonad: Monad[Validate] = monadEquivalence[Validate, \/[E, ?]]
  }

  import Validate._

  case class Validate[A](v: \/[E, A])

  def failure(e: E) = e.left
  def success[A](a: A) = a.right
  def success() = {}.right

  object LogValidate {
    implicit def equivalence = new Equivalence[LogValidate, WriterT[Validate, Vector[L], ?]] {
      override def get[A](t: LogValidate[A]): WriterT[Validate, Vector[L], A] = t.writer

      override def build[A](m: WriterT[Validate, Vector[L], A]): LogValidate[A] = LogValidate(m)
    }

    implicit def logValidate[A](v: Validate[A]): LogValidate[A] = LogValidate(implicitly[MonadTrans[WriterT[?[_], Vector[L], ?]]].liftMU(v))

    implicit val logValidateMonad: Monad[LogValidate] = monadEquivalence[LogValidate, WriterT[Validate, Vector[L], ?]]

    implicit def logValidateToStep[A](v: LogValidate[A]): Step[A] = implicitly[MonadTrans[StateT[?[_], S, ?]]].liftMU(v)

    implicit class LogValidateDecorator[A](l: LogValidate[A]) {
      def toStep: Step[A] = l
    }

  }

  case class LogValidate[A](writer: WriterT[Validate, Vector[L], A])

  object Step {
    type InnerStep[A] = StateT[LogValidate, S, A]

    implicit def equivalence = new Equivalence[Step, InnerStep] {
      override def get[A](t: Step[A]): InnerStep[A] = t.s

      override def build[A](m: InnerStep[A]): Step[A] = Step(m)
    }

    implicit def innerStateToStep[A](v: InnerStep[A]): Step[A] = Step(v)

    implicit def logToStep[A](l: Log[A]) = l.toStep

    implicit def stateToStep[A](v: State[S, A]): Step[A] = StateT[LogValidate, S, A] {
      s => v.run(s).point[LogValidate]
    }

    implicit def stateTValidateToStep[A](v: StateT[Validate, S, A]): Step[A] = StateT[LogValidate, S, A] { state =>
      LogValidate.logValidate(v.run(state))
    }

    implicit def stateTLogToStep[A](v: StateT[Log, S, A]): Step[A] = StateT[LogValidate, S, A] { state =>
      v.run(state).toValidate
    }

    implicit def monad = monadEquivalence[Step, InnerStep]

    def apply(): Step[S] = State.get[S]

  }

  case class Step[A](s: Step.InnerStep[A]) {
    def run(st: S) = s.run(st)
  }

  def step[A](a: Validate[A]): Step[A] = a.toStep
  def step[A](a: Log[A]): Step[A] = a.toStep
  def step[A](a: A): Step[A] = a.point[Step]

}