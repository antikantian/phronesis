package phronesis

import cats.Eval

package object collection {
  type LeftFold[-A, C] = (C, A) => C

  type RightFold[-A, C] = (A, Eval[C]) => Eval[C]
}
