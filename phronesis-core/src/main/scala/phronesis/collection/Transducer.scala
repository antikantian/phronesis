package phronesis
package collection

import cats.Monoid

trait Transducer[+A, -B] { self =>

  def apply[S](rf: Reducer[A, S]): Reducer[B, S]

  def andThen[C](that: Transducer[B, C]): Transducer[A, C] =
    that compose this

  def combine[C](that: Transducer[B, C]): Transducer[A, C] =
    new Transducer[A, C] {
      def apply[S](rf: Reducer[A, S]): Reducer[C, S] =
        that(self(rf))
    }

  def compose[C](that: Transducer[C, A]): Transducer[C, B] =
    new Transducer[C, B] {
      def apply[S](rf: Reducer[C, S]) = self(that(rf))
    }

  def transform[S](rf: Reducer[A, S]): Reducer[B, S] = self(rf)
}

object Transducer {
  def apply[A, B](f: A => B) =
    new Transducer[B, A] {
      def apply[S](rf: Reducer[B, S]): Reducer[A, S] = (s, a) => rf(s, f(a))
    }

  def filter[A](p: A => Boolean): Transducer[A, A] =
    new Transducer[A, A] {
      def apply[S](rf: Reducer[A, S]): Reducer[A, S] =
        (s, a) => if (p(a)) rf(s, a) else s
    }

  def map[A, B](f: B => A): Transducer[A, B] =
    new Transducer[A, B] {
      def apply[S](rf: Reducer[A, S]): Reducer[B, S] =
        (s, b) => rf(s, f(b))
    }

  // TODO: Replace with comonadic skip/halt instead of throwing exception?
  private[phronesis] object Skip extends Exception with scala.util.control.NoStackTrace
  private[phronesis] object Halt extends Exception with scala.util.control.NoStackTrace
}

private[phronesis] object Transducers {

  final class CombineTransducer[A, B, C](left: Transducer[A, B], right: Transducer[B, C]) extends Transducer[A, C] {
    def apply[S](rf: Reducer[A, S]): Reducer[C, S] =
      right(left(rf))
  }

}


