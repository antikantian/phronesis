package phronesis.syntax

trait TransducerSyntax {
  implicit def transducerOps[A](as: TraversableOnce[A]): TransducerOps[A] = new TransducerOps[A](as)
}

trait AllSyntax extends TransducerSyntax