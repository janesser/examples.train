package examples

package object train {

  type Speed = BigDecimal
  type Distance = BigDecimal
  type Time = BigDecimal

  def time(s: Speed, d: Distance): Time = d / s
}
