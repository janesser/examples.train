package examples.train

class SimpleScheduleSolver(override val network: Seq[Railway]) extends ScheduleSolver with NetworkAnalyser {
  import scala.language.postfixOps

  override def solve(trains: Seq[Train]): Map[Train, Seq[Railway]] = {
    require(trains.nonEmpty)

    val parts = connectedPartitions(network)
    require(parts.nonEmpty)
    require(parts.size <= trains.size)

    if (parts.size > 1)
      parts.zip(trains) map {
        case (part, train) =>
          train -> new SimpleScheduleSolver(part).solve(Seq(train)).head._2
      } toMap
    else
      Map(trains.head -> solve())
  }

  lazy val stations = network.map(_.s1) ++ network.map(_.s2) distinct

  def solve(unvisited: Seq[Station] = stations.tail, lastStation: Station = stations.head, route: Seq[Railway] = Seq()): Seq[Railway] = {
    if (unvisited.isEmpty) route
    else {
      val nextStation = unvisited.head
      val r = findRoute(lastStation, nextStation).get
      solve(unvisited.filter(_ != nextStation), nextStation, route ++ r)
    }
  }

}
