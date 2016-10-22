package phronesis

import cats._

trait PointSpace[A] {
  def dims: Dim
  def extract: A
  def container: Container[A]

  def map[B](f: A => B): PointSpace[B]
  def coflatMap[B](f: PointSpace[A] => B): PointSpace[B]
}

object PointSpace {

  implicit val pointSpaceComonad: Comonad[PointSpace] = new Comonad[PointSpace] {
    override def extract[A](fa: PointSpace[A]): A = fa.extract
    override def map[A, B](fa: PointSpace[A])(f: A => B): PointSpace[B] = fa map f
    override def coflatMap[A, B](fa: PointSpace[A])(f: PointSpace[A] => B): PointSpace[B] = fa coflatMap f
  }

}