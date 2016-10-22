package phronesis
package collection

import cats.arrow._
import cats.data._
import cats.functor._

abstract class Transducer[-A, +B] { self =>
  def left[C](r: LeftFold[B, C]): LeftFold[A, C]

  def right[C](t: RightFold[B, C]): RightFold[A, C]

  class Syllogism[C](that: Transducer[B, C]) extends Transducer[A, C] {
    def left[D](s: LeftFold[C, D]): LeftFold[A, D] = self.left(that.left(s))
    def right[D](s: RightFold[C, D]): RightFold[A, D] = self.right(that.right(s))

    override def fresh: Transducer[A, C] = {
      val self1 = self.fresh
      val that1 = that.fresh
      if ((self1 == self) && (that1 == that)) this
      else self1 andThen that1
    }
  }

  def andThen[C](that: Transducer[B, C]): Transducer[A, C] = {
    if (canAbort || that.canAbort)
      new Syllogism[C](that) {
        override def canAbort = true

        override def done = self.done || that.done
      }
    else
      new Syllogism[C](that)
  }

  @inline final def >>[C](that: Transducer[B, C]): Transducer[A, C] = andThen(that)

  def fresh: Transducer[A, B] = this

  def canAbort: Boolean = false

  def done: Boolean = false
}

object Transducer {

}

object TransducerTypes {

  abstract class CountingTransducer[A, B] extends Transducer[A, B] with Cloneable {
    var count = 0
    override def fresh = {
      val result = clone.asInstanceOf[CountingTransducer[A, B]]
      result.count = 0
      result
    }
  }

  class IdentityTransducer[A] extends Transducer[A, A] {
    def left[C](r: LeftFold[A, C]) = { (acc, elem) =>
      r(acc, elem)
    }

    def right[C](r: RightFold[A, C]) = { (elem, acc) =>
      r(elem, acc)
    }
  }

  class FilterTransducer[A](p: A => Boolean) extends Transducer[A, A] {
    def left[C](r: LeftFold[A, C]): LeftFold[A, C] =
      (acc: C, elem: A) => if (p(elem)) r(acc, elem) else acc

    def right[C](r: RightFold[A, C]): RightFold[A, C] =
      (elem, acc) => if (p(elem)) r(elem, acc) else acc
  }

  class FlatMapTransducer[A, B](f: A => Seq[B]) extends Transducer[A, B] {
    def left[C](r: LeftFold[B, C]): LeftFold[A, C] =
      (acc, elem) => f(elem).foldLeft(acc)(r)

    def right[C](r: RightFold[B, C]): RightFold[A, C] =
      (elem, acc) => f(elem).foldRight(acc)(r)
  }

  class MapTransducer[A, B](f: A => B) extends Transducer[A, B] {
    def left[C](r: LeftFold[B, C]): LeftFold[A, C] =
      (acc, elem) => r(acc, f(elem))

    def right[C](r: RightFold[B, C]): RightFold[A, C] =
      (elem, acc) => r(f(elem), acc)
  }

  class SliceTransducer[A](start: Int, end: Int) extends CountingTransducer[A, A] {
    def left[C](r: LeftFold[A, C]): LeftFold[A, C] = { (acc, elem) =>
      if (count >= end) acc
      else {
        count += 1
        if (count < start) acc
        else r(acc, elem)
      }
    }

    def right[C](r: RightFold[A, C]): RightFold[A, C] = { (elem, acc) =>
      if (count >= end) acc
      else {
        count += 1
        if (count < start) acc
        else r(elem, acc)
      }
    }
    override def canAbort = true
    override def done = count >= end
  }

  class TakeWhileTransducer[A](p: A => Boolean) extends CountingTransducer[A, A] {
    def left[C](r: LeftFold[A, C]): LeftFold[A, C] = { (acc, elem) =>
      count = if (count >= 0 && !p(elem)) -(count + 1) else count + 1
      if (count >= 0) r(acc, elem)
      else acc
    }

    def right[C](r: RightFold[A, C]): RightFold[A, C] = { (elem, acc) =>
      count = if (count >= 0 && !p(elem)) -(count + 1) else count + 1
      if (count >= 0) r(elem, acc)
      else acc
    }
    override def canAbort = true
    override def done = count < 0
  }

  class DropWhileTransducer[A](p: A => Boolean) extends CountingTransducer[A, A] {
    def left[C](r: LeftFold[A, C]): LeftFold[A, C] = { (acc, elem) =>
      count = if (count >= 0 && !p(elem)) -(count + 1) else count + 1
      if (count < 0) r(acc, elem)
      else acc
    }

    def right[C](r: RightFold[A, C]): RightFold[A, C] = { (elem, acc) =>
      count = if (count >= 0 && !p(elem)) -(count + 1) else count + 1
      if (count > 0) r(elem, acc)
      else acc
    }
  }

  class ZipWithIndex[A]() extends CountingTransducer[A, (A, Int)] {
    def left[C](r: LeftFold[(A, Int), C]): LeftFold[A, C] = { (acc, elem) =>
      count += 1
      r(acc, (elem, count - 1))
    }

    def right[C](r: RightFold[(A, Int), C]): RightFold[A, C] = { (elem, acc) =>
      count += 1
      r((elem, count - 1), acc)
    }
  }

}

trait TransducerInstances0 extends TransducerInstances1{
  import TransducerTypes._

//  implicit val transducerBifunctor: Bifunctor[Transducer] = new Bifunctor[Transducer] {
//    def bimap[A, B, C, D](fab: Transducer[A, B])(f: A => C, g: B => D): Transducer[C, D] = {
//      val fa = new MapTransducer(f)
//      val fb = new MapTransducer(g)
//      val fc = new IdentityTransducer[C]
//      fa.andThen(fc)
//
//    }
//  }
}

trait TransducerInstances1 {
  import TransducerTypes._

  implicit val transducerCategory: Category[Transducer] = new Category[Transducer] {
    def compose[A, B, C](f: Transducer[B, C], g: Transducer[A, B]): Transducer[A, C] =
      g andThen f

    def id[A]: Transducer[A, A] = new IdentityTransducer[A]
  }
}







