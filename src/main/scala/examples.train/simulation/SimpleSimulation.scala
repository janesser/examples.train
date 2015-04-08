package examples.train.simulation

import examples.train._

class SimpleSimulator extends Simulator[SimpleState] {

  override def simulate(schedules: Map[Train, Seq[Railway]]): Stream[SimpleState] = {
    def stateStream:Stream[SimpleState] =
      SimpleState(0, schedules) #:: stateStream.map(_.step())
    stateStream
  }
}

case class SimpleState(stepCount: Int,
                       schedules: Map[Train, Seq[Railway]])
                      (implicit listeners: Seq[Listener[SimpleState]]) extends State {

  def step(): SimpleState = {
    listeners foreach {
      _.beforeStep(this)
    }

    val nextSchedules = schedules map {
      case (t, route) =>
        listeners foreach {
          _.onTrainMove(this, t, route.head)
        }
        t -> route.tail
    } filter {
      case (t, route) =>
        route.nonEmpty
    } toMap

    SimpleState(stepCount + 1, nextSchedules)
  }

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

class SimulationAnalyser extends Listener[State] {
  var work: Map[Train, Time] = Map().withDefaultValue(BigDecimal(0))

  override def onTrainMove(state: State, t: Train, r: Railway): Unit =
    work +=
      t -> (work(t) + time(t.spd, r.d))

  def score: Time =
    work.values.foldLeft(BigDecimal(0)) {
      (acc, w) =>
        acc + w
    }
}