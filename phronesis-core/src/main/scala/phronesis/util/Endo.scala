package phronesis.util

import cats.Monoid
import cats.functor.Invariant

/** Adapted with only minor modifications from:
 * https://github.com/scalaz/scalaz/blob/series/7.3.x/core/src/main/scala/scalaz/Endo.scala
 */
final case class Endo[A](run: A => A) {
  def apply(a: A): A = run(a)
  def compose(other: Endo[A]): Endo[A] = Endo.endo(run compose other.run)
  def andThen(other: Endo[A]): Endo[A] = other compose this
}

object Endo extends EndoInstances {
  final def endo[A](f: A => A): Endo[A] = Endo(f)

  final def constantEndo[A](a: => A): Endo[A] = endo[A](_ => a)

  final def idEndo[A]: Endo[A] = endo[A](a => a)
}

sealed abstract class EndoInstances {
  implicit def endoMonoid[A]: Monoid[Endo[A]] = new Monoid[Endo[A]] {
    def combine(x: Endo[A], y: Endo[A]): Endo[A] = x compose y
    def empty: Endo[A] = Endo.idEndo
  }

  implicit val endoInvariant: Invariant[Endo] = new Invariant[Endo] {
    def imap[A, B](fa: Endo[A])(f: A => B)(g: B => A): Endo[B] = {
      Endo.endo(g andThen fa.run andThen f)
    }
  }


}