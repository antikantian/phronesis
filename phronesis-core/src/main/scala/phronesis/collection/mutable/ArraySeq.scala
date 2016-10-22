package phronesis
package collection.mutable

import phronesis.collection.immutable

class ArraySeq[@sp A](private[collection] override val elems: Array[A]) extends immutable.ArraySeq[A](elems) {
  def update(i: Int, value: A): Unit = elems.update(i, value)
}

object ArraySeq extends immutable.ArraySeqInstances