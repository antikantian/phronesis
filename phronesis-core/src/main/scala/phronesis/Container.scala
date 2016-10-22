package phronesis

import cats._
import cats.arrow._
import cats.data._

trait Container[A] {
  def extract: A

  def map[B](f: A => B): Container[B]

  def coflatMap[B](f: Container[A] => B): Container[B]
}

object Container extends ContainerInstances {
  type Context[A, B] = Cokleisli[Container, A, B]

  def extract[A]: Cokleisli[Container, A, A] =
    Cokleisli[Container, A, A](_.extract)
}

abstract sealed class ContainerInstances {
  implicit val containerComonad: Comonad[Container] = new Comonad[Container] {
    override def extract[A](fa: Container[A]): A = fa.extract
    override def map[A, B](fa: Container[A])(f: A => B): Container[B] = fa map f
    override def coflatMap[A, B](fa: Container[A])(f: Container[A] => B): Container[B] = fa coflatMap f
  }

  implicit val fromPointSpace: PointSpace ~> Container = new FunctionK[PointSpace, Container] {
    def apply[A](ps: PointSpace[A]): Container[A] = ps.container
  }
}