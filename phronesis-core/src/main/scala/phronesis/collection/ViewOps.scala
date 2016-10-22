package phronesis
package collection

import scala.collection.generic.CanBuild
import scala.collection.mutable.ListBuffer

import cats.Eval

abstract class ViewOps[B] {
  import TransducerTypes._
  import ViewOps._

  type A

  val source: Seq[A]

  def transducer: Transducer[A, B]

  def hasNext: Boolean = transducer.isInstanceOf[IdentityTransducer[_]]

  def andThen[C](t: Transducer[B, C]): ViewOps[C] =
    new Link(source, transducer andThen t)

  // view -> view
  def map[C](f: B => C): ViewOps[C] = andThen(new MapTransducer(f))

  def filter(p: B => Boolean): ViewOps[B] = andThen(new FilterTransducer(p))

  def flatMap[C](f: B => Seq[C]): ViewOps[C] = andThen(new FlatMapTransducer(f))

  def slice(from: Int, end: Int): ViewOps[B] = andThen(new SliceTransducer(from, end))

  def take(n: Int): ViewOps[B] = slice(0, n)

  def drop(n: Int): ViewOps[B] = slice(n, Int.MaxValue)

  def takeWhile(p: B => Boolean): ViewOps[B] = andThen(new TakeWhileTransducer(p))

  def dropWhile(p: B => Boolean): ViewOps[B] = andThen(new DropWhileTransducer(p))

  def zipWithIndex = andThen(new ZipWithIndex[B])

  def partition(p: B => Boolean): (ViewOps[B], ViewOps[B]) = (filter(p), filter(!p(_)))

  // view -> X
  @tailrec private def combineLeft[R](xs: Seq[A], r: R, f: (R, A) => R, done: R => Boolean): R =
    if (xs.isEmpty || done(r)) r
    else combineLeft(xs.tail, f(r, xs.head), f, done)

  private def combineRight[C](xs: Seq[A], lc: Eval[C], f: (A, Eval[C]) => Eval[C]): Eval[C] = {
    def loop(i: Int): Eval[C] =
      if (i < xs.length) f(xs(i), Eval.defer(loop(i + 1))) else lc
    Eval.defer(loop(0))
  }

  def foldLeft[C](c: C)(f: (C, B) => C): C = {
    val trans = transducer.fresh
    combineLeft[C](source, c, trans.left(f), _ => trans.done)
  }

  def foldLeft[C](c: C, done: C => Boolean)(f: (C, B) => C): C = {
    val trans = transducer.fresh
    combineLeft[C](source, c, trans.left(f), c => done(c) || trans.done)
  }

  def foldRight[C](lc: Eval[C])(f: (B, Eval[C]) => Eval[C]): Eval[C] = {
    val trans = transducer.fresh
    combineRight(source, lc, trans.right(f))
  }

  def indexOf(p: B => Boolean): Int =
    zipWithIndex.foldLeft(-1, (_: Int) >= 0) { (acc, xn) =>
      val (x, n) = xn
      if (p(x)) n else acc
    }

  def find(p: B => Boolean): Option[B] =
    foldLeft(None: Option[B], (_: Option[B]).isDefined)((acc, elem) => if (p(elem)) Some(elem) else acc)

  def to[Col[_]](implicit cb: CanBuild[B, Col[B @uV]]): Col[B @uV] = {
    val builder = cb()
    builder.sizeHint(source.size)
    foldLeft(builder) { (acc, elem) => acc += elem }
    builder.result
  }

  def toList: List[B] = foldLeft(new ListBuffer[B])(_ += _).toList

  def toStream: Stream[B] = foldRight(Eval.now(Stream.Empty: Stream[B])) { (elem, acc) =>
    acc.map(s => elem #:: s)
  }.value

  override def toString = s"ViewStack($source, $transducer, End)"
}

object ViewOps {
  def apply[B](bs: B*): ViewOps[B] =
    new Link[B, B](bs, new TransducerTypes.IdentityTransducer[B])

  private[phronesis] final class Link[From, To](
      override val source: Seq[From],
      override val transducer: Transducer[From, To])
    extends ViewOps[To] { type A = From }
}
