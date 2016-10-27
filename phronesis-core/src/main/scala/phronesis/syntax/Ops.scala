package phronesis.syntax

import phronesis.collection._
import phronesis.collection.internal._

final class TraversableOps[A](val as: TraversableOnce[A]) extends AnyVal {
  def transduce[B](t: Transducer[B, A]) = new Reduced(as, t)
  def transduceC[B](t: Transducer[B, A]) = new ReducedC(as, t)
}

final class TransducerOps[A, B](val t1: Transducer[A, B]) {
  def compose[C](t2: Transducer[C, A]): Transducer[C, B] = t1 compose t2
  def transform[S](rf: Reducer[A, S]): Reducer[B, S] = t1 transform rf
}