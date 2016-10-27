package phronesis.collection
package internal

import scala.collection.generic.CanBuildFrom

import cats.Monoid
import cats.implicits._
import spire.algebra.{ AdditiveMonoid, MultiplicativeMonoid }
import spire.implicits._

trait ReducedOps[A, B] {
  def fold[S](init: S)(rf: Reducer[B, S]): S

  def to[Col[_]](implicit cbf: CanBuildFrom[Col[B], B, Col[B]]): Col[B] =
    fold(cbf())(_ += _).result

  def reduce(implicit B: Monoid[B]): B =
    fold(B.empty)(_ |+| _)

  def sum(implicit B: AdditiveMonoid[B]): B =
    fold(B.zero)(_ + _)

  def product(implicit B: MultiplicativeMonoid[B]): B =
    fold(B.one)(_ * _)
}