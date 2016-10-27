package phronesis.collection
package internal

import cats.data.State

object reducers {
  final class CountingReducer[A] extends Reducer[A, Int] {
    override def apply(v1: Int, v2: A): Int = v1 + 1
  }

  final class StatefulReducer[A] extends State[]

}