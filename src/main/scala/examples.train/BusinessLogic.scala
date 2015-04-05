package examples.train

import scala.language.postfixOps

trait NetworkAnalyser {
  def network: Seq[Railway]

  require(network.nonEmpty)

  def findRoute(s1: Station, s2: Station, part: Seq[Railway] = network): Option[Seq[Railway]] = {
    def findWays(candidateRoutesAcc: Map[Station, Seq[Railway]] = Map(),
                 unvisitedAcc: Seq[Railway] = part): Option[Seq[Railway]] = {
      candidateRoutesAcc.get(s2) match {
        case route: Some[Seq[Railway]] => route
        case None =>
          // evolve candidates by one way
          val nextGen = candidateRoutesAcc.foldLeft((candidateRoutesAcc, unvisitedAcc)) {
            case (acc, candidateRoute: (Station, Seq[Railway])) =>
              acc match {
                case (candidateRoutes, unvisited) =>
                  val ways = connectedWays(candidateRoute._1, unvisited)

                  if (ways.isEmpty) {
                    // failed candidate
                    (candidateRoutes - candidateRoute._1, unvisited)
                  } else {
                    val nextUnvisited = unvisited.filterNot(ways.contains(_))
                    val nextCandidateRoutes = ways map {
                      w =>
                        w.outer(candidateRoute._1) -> (candidateRoute._2 :+ w)
                    } toMap

                    (candidateRoutes ++ nextCandidateRoutes, nextUnvisited)
                  }
              }
          }

          if (nextGen._1.isEmpty) None
          else findWays(nextGen._1, nextGen._2)
      }
    }

    findWays(Map(s1 -> Seq()))
  }

  def connectedWays(s: Station, part: Seq[Railway] = network): Seq[Railway] =
    part.filter(_.connectedTo(s))

  protected def visitAll(unvisited: Seq[Railway], heads: Set[Station]): Seq[Railway] = {
    if (unvisited.isEmpty || heads.isEmpty)
      unvisited
    else {
      val h = heads.head
      val ways = connectedWays(h, unvisited)
      if (ways.isEmpty)
        visitAll(unvisited, heads.tail)
      else {
        visitAll(unvisited.filter(!ways.contains(_)), heads ++ ways.map(_.s1) ++ ways.map(_.s2))
      }
    }
  }

  def isConnected(part: Seq[Railway] = network): Boolean =
    visitAll(part, Set(part.head.s1)).isEmpty

  def connectedPartitions(part: Seq[Railway] = network, acc: Seq[Seq[Railway]] = Seq()): Seq[Seq[Railway]] = {
    if (part.isEmpty) acc
    else connectedPartitions(part.tail,
      if (acc.isEmpty)
        Seq(Seq(part.head))
      else {
        val w = part.head

        if (isConnected(acc.flatten :+ w))
          acc map {
            connectedWays =>
              if (isConnected(connectedWays :+ w))
                connectedWays :+ w
              else
                connectedWays
          }
        else
          acc :+ Seq(w)
      })
  }
}

trait ScheduleSolver extends NetworkAnalyser {
  def solve(trains: Seq[Train], part: Seq[Railway] = network): Map[Train, Seq[Railway]]
}

trait ScheduleAnalyser {
  this: NetworkAnalyser =>

  protected def validRoute(route: Seq[Railway]): Boolean = {
    if (route.size <= 1)
      true
    else {
      val first: Station =
        route.head.outer(route.tail.head)
      val last: Station =
        route.last.outer(route.reverse.tail.head)

      findRoute(first, last, route).nonEmpty
    }

  }

  protected def coversAllStations(stations: Set[Station], routes: Map[Train, Seq[Railway]]): Boolean = {
    stations map {
      s => s -> routes.valuesIterator.flatten.count(r => r.s1 == s || r.s2 == s)
    } filter {
      case (station, visitCount) =>
        visitCount == 0
    } isEmpty
  }

  def measure(stations: Seq[Station], routes: Map[Train, Seq[Railway]]): Time =
    measure(stations.toSet, routes)

  def measure(stations: Set[Station], routes: Map[Train, Seq[Railway]]): Time = {
    require(coversAllStations(stations, routes))
    routes map {
      case (train, route) =>
        measure(train.spd, route)
    } max
  }

  protected def measure(s: Speed, route: Seq[Railway]): Time = {
    require(s > 0)
    require(validRoute(route))
    route.foldLeft(BigDecimal(0))((sum, way) => sum + time(s, way.d))
  }
}
