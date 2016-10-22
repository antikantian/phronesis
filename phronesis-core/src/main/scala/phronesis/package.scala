package object phronesis {

  private[phronesis] type uV = scala.annotation.unchecked.uncheckedVariance
  private[phronesis] type sp = scala.specialized
  private[phronesis] type ClassTag[A] = scala.reflect.ClassTag[A]
  private[phronesis] type tailrec = scala.annotation.tailrec
  private[phronesis] val ClassTag = scala.reflect.ClassTag

}