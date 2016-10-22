package phronesis

trait Agg[+A] {
  def apply(i: Int): A
  def size: Int
  def isEmpty: Boolean = size == 0
}