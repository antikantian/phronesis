package phronesis
package collection
package internal

import cats.Comonad
import cats.data.Cokleisli

class Reduced[A, B](as: TraversableOnce[A], t: Transducer[B, A]) extends ReducedOps[A, B] {
  def fold[R](init: R)(rf: Reducer[B, R]): R =
    as.foldLeft(init)(t(rf))
}

class ReducedC[A, B](as: TraversableOnce[A], t: Transducer[B, A]) extends ReducedOps[A, B] {
  def fold[R](init: R)(rf: Reducer[B, R]): R = {
    val it = as.toIterator
    val f: Reducer[A, R] = t(rf)
    var result: R = init
    while (it.hasNext) {
      try {
        result = f(result, it.next)
      } catch {
        case _: Transducer.Skip.type => ()
        case _: Transducer.Halt.type => return result
      }
    }
    result
  }
}

class ReducedCC[A, B](as: TraversableOnce[A], t: Transducer[B, A]) extends ReducedOps[A, B] {
  def fold[R](init: R)(rf: Reducer[B, R]): R = {
    val f: Reducer[A, R] = t(rf)
    @tailrec def go(it: Iterator[A], res: R): R = {
      if (it.hasNext) go(it, f(res, it.next))
      else res
    }
    go(as.toIterator, init)
  }
}

object Reduced {

}