package examples

package object train {

  implicit class RemoveAllSeq[T](s1: Seq[T]) {
    def --(s2: Seq[T]): Seq[T] = s1.filterNot(e => s2.contains(e))
  }

  type Speed = BigDecimal
  type Distance = BigDecimal
  type Time = BigDecimal

  def time(s: Speed, d: Distance): Time = d / s
}
