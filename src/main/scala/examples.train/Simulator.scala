package examples.train

class Simulator {

  case class SState(override val stepCount: Int,
                    override val schedules: Map[Train, Seq[Railway]]) extends SimulationState {

    override def step(): SimulationState = {
      val nextSchedules = schedules map {
        case (t, route) =>
          listeners.foreach {
            _.onTrainMove(this, t, route.head)
          }
          t -> route.tail
      } filter {
        case (t, route) =>
          route.nonEmpty
      }

      SState(stepCount + 1, nextSchedules)
    }
  }

  val listeners = scala.collection.mutable.Buffer.empty[SimulationListener]

  def simulate(schedules: Map[Train, Seq[Railway]]): Stream[SimulationState] = {
    val state = SState(0, schedules)

    state #:: nextStep(state)
  }

  def nextStep(state: SimulationState): Stream[SimulationState] =
    if (state.hasMore) Stream(state.step())
    else Stream.empty
}

trait SimulationState {
  def stepCount: Int

  def step(): SimulationState

  def schedules: Map[Train, Seq[Railway]]

  def trains = schedules.keySet

  def locate(t: Train) =
    schedules.get(t) match {
      case Some(route) =>
        route.headOption
      case None =>
        None
    }

  def hasMore: Boolean =
    schedules.nonEmpty

}

class SimulationException extends Exception

trait SimulationListener {
  @throws[SimulationException]
  def onTrainMove(state: SimulationState, t: Train, r: Railway): Unit
}

