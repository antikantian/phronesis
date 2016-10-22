package phronesis
package collection.immutable

import scala.collection.mutable

import cats._
import spire.implicits.cforRange

class ArraySeq[@sp A](private[collection] val elems: Array[A]) {

  def +:(elem: A): ArraySeq[A] = {
    val resultSize = this.length + 1
    val result = java.lang.reflect.Array.newInstance(this.getClass.getComponentType, resultSize).asInstanceOf[Array[A]]
    result.update(0, elem)
    System.arraycopy(this.elems, 0, result, 1, this.elems.length)
    new ArraySeq[A](result)
  }

  def ++(that: ArraySeq[A]): ArraySeq[A] =
    if (this.isEmpty) that
    else if (that.isEmpty) this
    else {
      val resultSize = this.length + that.length
      val result = java.lang.reflect.Array.newInstance(this.getClass.getComponentType, resultSize).asInstanceOf[Array[A]]
      System.arraycopy(this.elems, 0, result, 0, this.elems.length)
      System.arraycopy(that.elems, 0, result, this.elems.length, that.elems.length)
      new ArraySeq[A](result)
    }

  def ===(that: ArraySeq[A])(implicit A: Eq[A]): Boolean = {
    @tailrec def loop(curr: Int, last: Int, a: ArraySeq[A], b: ArraySeq[A]): Boolean = {
      if (curr < last) {
        val acurr = a(curr)
        val bcurr = b(curr)
        if (A.eqv(acurr, bcurr)) loop(curr + 1, last, a, b)
        else false
      } else true
    }
    if (this.length == that.length)
      loop(0, that.length, this, that)
    else false
  }

  def eqv(that: ArraySeq[A])(implicit A: Eq[A]): Boolean = {
    if (this.length == that.length) {
      cforRange(0 until this.length) { i =>
        if (A.neqv(this(i), that(i))) return false
      }
      true
    } else false
  }

  def apply(i: Int): A = elems(i)

  def get(i: Int): Option[A] = elems.lift(i)

  def getOrElse[B >: A](i: Int, default: => B): B =
    elems.lift(i).getOrElse(default)

  def isEmpty: Boolean = elems.isEmpty

  def length: Int = elems.length

  def map[@sp B: ClassTag](f: A => B): ArraySeq[B] =
    new ArraySeq[B](this.elems map f)

  def flatMap[@sp B: ClassTag](f: A => ArraySeq[B]): ArraySeq[B] =
    new ArraySeq[B](this.elems.flatMap(f(_).elems))

}

object ArraySeq extends ArraySeqInstances {
  def apply[@sp A: ClassTag](as: A*): ArraySeq[A] = {
    val result = implicitly[ClassTag[A]].newArray(as.length)
    as.copyToArray(result)
    new ArraySeq[A](result)
  }

  def empty[@sp A: ClassTag]: ArraySeq[A] = new ArraySeq(Array.empty[A])

}

private[phronesis] trait ArraySeqInstances extends ArraySeqInstancesForCats

private[phronesis] sealed trait ArraySeqInstancesForCats {
  implicit val arraySeqInstances: TraverseFilter[ArraySeq] with MonadCombine[ArraySeq] with CoflatMap[ArraySeq] =
    new TraverseFilter[ArraySeq] with MonadCombine[ArraySeq] with CoflatMap[ArraySeq] {
      def empty[A]: ArraySeq[A] = ArraySeq.empty[A]

      def combineK[A](x: ArraySeq[A], y: ArraySeq[A]): ArraySeq[A] = x ++ y

      def pure[A](x: A): ArraySeq[A] = ArraySeq(x)

      override def map[A, B](fa: ArraySeq[A])(f: A => B): ArraySeq[B] =
        fa map f

      def flatMap[A, B](fa: ArraySeq[A])(f: A => ArraySeq[B]): ArraySeq[B] =
        fa flatMap f

      override def map2[A, B, Z](fa: ArraySeq[A], fb: ArraySeq[B])(f: (A, B) => Z): ArraySeq[Z] =
        fa.flatMap(a => fb.map(b => f(a, b)))

      def coflatMap[A, B](fa: ArraySeq[A])(f: ArraySeq[A] => B): ArraySeq[B] = {
        @tailrec def loop(builder: mutable.ArrayBuilder[B], as: ArraySeq[A]): ArraySeq[B] =
          as match {
            case _ +: rest => loop(builder += f(as), rest)
            case _ => new ArraySeq(builder.result())
          }
        loop(mutable.ArrayBuilder.make[B], fa)
      }

      def foldLeft[A, B](fa: ArraySeq[A], b: B)(f: (B, A) => B): B = fa.elems.foldLeft[B](b)(f)

      def foldRight[A, B](fa: ArraySeq[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]) = {
        def loop(i: Int): Eval[B] =
          if (i < fa.length) f(fa.elems(i), Eval.defer(loop(i + 1))) else lb
        Eval.defer(loop(0))
      }

      def traverseFilter[G[_], A, B](fa: ArraySeq[A])(f: A => G[Option[B]])(implicit G: Applicative[G]): G[ArraySeq[B]] =
        foldRight[A, G[ArraySeq[B]]](fa, Always(G.pure(ArraySeq.empty))) { (a, lg) =>
          G.map2Eval(f(a), lg)((ob, as) => ob.fold(as)(_ +: as))
        }.value

      def tailRecM[A, B](a: A)(fn: A => ArraySeq[Either[A, B]]): ArraySeq[B] = {
        val buf = mutable.ArrayBuilder.make[B]
        var state = List(fn(a).elems.iterator)
        @tailrec def loop(): Unit = state match {
          case Nil => ()
          case h :: tail if h.isEmpty =>
            state = tail
            loop()
          case h :: tail =>
            h.next match {
              case Right(b) =>
                buf += b
                loop()
              case Left(a1) =>
                state = fn(a).elems.iterator :: h :: tail
                loop()
            }
        }
        loop()
        new ArraySeq(buf.result())
      }

      override def size[A](fa: ArraySeq[A]): Long = fa.length.toLong

      override def isEmpty[A](fa: ArraySeq[A]): Boolean = fa.isEmpty

    }

  implicit def arraySeqShow[A: Show]: Show[ArraySeq[A]] = new Show[ArraySeq[A]] {
    def show(fa: ArraySeq[A]): String =
      fa.elems.mkString("ArraySeq(", ", ", ")")
  }
}

