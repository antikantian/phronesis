package phronesis.collection.internal

import cats.Comonad
import cats.data.Cokleisli

case class Continue[A](extract: A) {
  def coflatMap[B](f: Continue[A] => B): Continue[B] =
    Continue(f(this))

  def map[B](f: A => B): Continue[B] =
    Continue(f(extract))

  override def toString = "Continue"
}

object Continue {
  implicit val contComonad: Comonad[Continue] = new Comonad[Continue] {
    override def extract[A](fa: Continue[A]): A = fa.extract
    override def map[A, B](fa: Continue[A])(f: A => B): Continue[B] = fa map f
    override def coflatMap[A, B](fa: Continue[A])(f: Continue[A] => B): Continue[B] = fa coflatMap f
  }
}