package examples.train

case class Train(id: String, spd: Speed)

case class Station(id: String)

case class Railway(s1: Station, s2: Station, d: Distance) {
  def connectedTo(s:Station):Boolean = s == s1 || s == s2

  def outer(proxy:Railway):Station =
    if (proxy.connectedTo(s1)) s2
    else s1

  def outer(s:Station):Station =
    if (s1 == s) s2
    else s1
}
