package examples.train

class SimpleScheduleSolver(override val network: Seq[Railway]) extends ScheduleSolver with NetworkAnalyser {
  import scala.language.postfixOps

  override def solve(trains: Seq[Train], part: Seq[Railway] = network): Map[Train, Seq[Railway]] = {
    require(trains.nonEmpty)

    val parts = connectedPartitions(part)
    require(parts.nonEmpty)
    require(parts.size <= trains.size)

    if (parts.size > 1)
      parts.zip(trains) map {
        case (partition, train) =>
          train -> solve(Seq(train), partition).head._2
      } toMap
    else {
      val stations = (part.map(_.s1) ++ part.map(_.s2)).distinct
      Map(trains.head -> scheduleTrain(stations.tail, stations.head))
    }
  }

  def scheduleTrain(unvisited: Seq[Station],
            lastStation: Station,
            route: Seq[Railway] = Seq()): Seq[Railway] = {
    if (unvisited.isEmpty) route
    else {
      val nextStation = unvisited.head
      val r = findRoute(lastStation, nextStation).get
      scheduleTrain(unvisited.filter(_ != nextStation), nextStation, route ++ r)
    }
  }

}
