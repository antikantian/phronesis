package phronesis
package collection

import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.collection.{GenTraversable, GenTraversableOnce}
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

/** Adapted from: https://gist.github.com/odersky/6b7c0eb4731058803dfd */
abstract class View[B] { self =>
  import TransducerTypes._

  type A

  val source: Seq[A]

  def transducer: Transducer[A, B]

  def andThen[C](t: Transducer[B, C]) = new View[C] {
    type A = self.A
    val source = self.source
    def transducer = self.transducer andThen t
  }

  @tailrec private def combineLeft[R](xs: Seq[A], r: R, f: (R, A) => R, done: R => Boolean): R =
    if (xs.isEmpty || done(r)) r
    else combineLeft(xs.tail, f(r, xs.head), f, done)

  private def combineRight[R](xs: Seq[A], r: R, f: (A, => R) => R): R =
    if (xs.isEmpty) r
    else f(xs.head, combineRight(xs.tail, r, f))

  def foldLeft[C](c: C)(f: (C, B) => C): C = {
    val trans = transducer.fresh
    combineLeft[C](source, c, trans.left(f), _ => trans.done)
  }

  def foldLeft[C](c: C, done: C => Boolean)(f: (C, B) => C): C = {
    val trans = transducer.fresh
    combineLeft[C](source, c, trans.left(f), c => done(c) || trans.done)
  }

  def foldRight[C](c: => C)(f: (B, => C) => C): C = {
    val trans = transducer.fresh
    combineRight(source, c, trans.right(f))
  }

  // view -> view
  def map[C](f: B => C): View[C] = andThen(new MapTransducer(f))

  def filter(p: B => Boolean): View[B] = andThen(new FilterTransducer(p))

  def flatMap[C](f: B => View[C]): View[C] = andThen(new FlatMapTransducer(f))

  def slice(from: Int, end: Int): View[B] = andThen(new SliceTransducer(from, end))

  def take(n: Int): View[B] = slice(0, n)

  def drop(n: Int): View[B] = slice(n, Int.MaxValue)

  def takeWhile(p: B => Boolean): View[B] = andThen(new TakeWhileTransducer(p))

  def dropWhile(p: B => Boolean): View[B] = andThen(new DropWhileTransducer(p))

  def zipWithIndex = andThen(new ZipWithIndex[B])

  def partition(p: B => Boolean): (View[B], View[B]) = (filter(p), filter(!p(_)))

  // view -> X
  def indexOf(p: B => Boolean): Int =
    zipWithIndex.foldLeft(-1, (_: Int) >= 0) { (acc, xn) =>
      val (x, n) = xn
      if (p(x)) n else acc
    }

  def find(p: B => Boolean): Option[B] =
    foldLeft(None: Option[B], (_: Option[B]).isDefined)((acc, elem) => if (p(elem)) Some(elem) else acc)

  def toList: List[B] = foldLeft(new ListBuffer[B])(_ += _).toList

  def toStream: Stream[B] = foldRight(Stream.Empty: Stream[B])(_ #:: _)

  override def toString = s"View(${toList mkString ", "})"

}

object View {

  def apply[T](xs: Seq[T]): View[T] = new View[T] {
    type A = T
    val source = xs
    def transducer = new TransducerTypes.IdentityTransducer
  }

}