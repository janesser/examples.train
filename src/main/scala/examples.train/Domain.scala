package examples.train

case class Train(id: String, speed: Speed)

case class Station(id: String)

case class Railway(s1: Station, s2: Station, d: Distance) {
  def connectedTo(s:Station):Boolean = s == s1 || s == s2

  def outer(succ:Railway):Station =
    if (succ.connectedTo(s1)) s2
    else s1

}
