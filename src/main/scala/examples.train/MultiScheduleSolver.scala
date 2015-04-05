package examples.train

class MultiScheduleSolver(override val network: Seq[Railway]) extends SimpleScheduleSolver(network) with ScheduleAnalyser {

  override def solve(trains: Seq[Train], part: Seq[Railway] = network): Map[Train, Seq[Railway]] = {
    val schedules = super.solve(trains, part)

    val remainingTrains = trains filter { t =>
      schedules.get(t).isEmpty
    }

    optimizeSchedule(schedules, remainingTrains)
  }

  def optimizeSchedule(schedules: Map[Train, Seq[Railway]],
                       remainingTrains: Seq[Train]): Map[Train, Seq[Railway]] = {
    if (remainingTrains.isEmpty)
      schedules
    else {
      // split longest and recur
      val lasting = schedules maxBy {
        case (t, route) =>
          measure(t.spd, route)
      }
      def routeTrain: Train = lasting._1
      def routeSplit: Seq[Railway] = {
        val route = lasting._2
        val routeHalfLength = route.size / 2
        route.take(routeHalfLength) ++ route.drop(routeHalfLength + 1)
      }

      val optSchedules =
        schedules ++
          solve(
            Seq(remainingTrains.head, routeTrain),
            routeSplit)
      optimizeSchedule(optSchedules, remainingTrains.tail)
    }
  }
}
