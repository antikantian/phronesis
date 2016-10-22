package phronesis

case class Dim(one: Int, two: Int = 1, three: Int = 1, four: Int = 1) {
  def elements: Int = one * two * three * four

  def ndims: Int = {
    val num = elements
    if (num == 0) 0
    else if (num == 1) 1
    else if (four != 1) 4
    else if (three != 1) 3
    else if (two != 1) 2
    else 1
  }
}