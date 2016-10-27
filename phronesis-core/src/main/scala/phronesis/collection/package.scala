package phronesis

import cats.data.State

package object collection {
  type Endo[A] = (A) => A
  type Reducer[-A, S] = (S, A) => S
}