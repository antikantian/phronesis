//package phronesis.collection.view
//
//abstract class ParView[B] extends View[B] {
//
//  def shouldSplit: Boolean = ???
//
//  def split: (ParView[B], ParView[B]) = ???
//
//  def inParallel[T, U](op1: T, op2: U): (T, U) = ???
//
//  def fold[C](c: C)(leftOp: (C, B) => C)(assocOp: (C, C) => C): C = {
//    if (shouldSplit) {
//      val (lv, rv) = split
//      val (l, r) = inParallel(lv.fold(c)(leftOp)(assocOp), rv.fold(c)(leftOp)(assocOp))
//      assocOp(l, r)
//    }
//    else foldLeft(c)(leftOp)
//  }
//
//  def reduce(b: B)(op: (B, B) => B): B = fold(b)(op)(op)
//
//  def toVector: Vector[B] =
//    fold(Vector.empty[B])(_ :+ _)(_ ++ _)
//}
//
//object ParView {
//  implicit class Summable(val pv: ParView[Int]) extends AnyVal {
//    def sum = pv.reduce(0)(_ + _)
//  }
//}
//
