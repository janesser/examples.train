package examples.train.simulation

import examples.train._

class SimpleSimulator extends Simulator[SimpleState] {

  def simulationStream(schedules: Map[Train, Seq[Railway]], stepCount: Int = 0): Stream[SimpleState] =
    SimpleState(stepCount, schedules) #:: simulationStream(schedules map {
      case (t, route) =>
        t -> route.tail
    } filter {
      case (t, route) =>
        route.nonEmpty
    }, stepCount + 1)

  override def simulate(schedules: Map[Train, Seq[Railway]]) =
    simulationStream(schedules)
}

case class SimpleState(stepCount: Int,
                       schedules: Map[Train, Seq[Railway]])
                      (implicit listeners: Seq[Listener[SimpleState]]) extends State {

  listeners foreach { l =>
    l.beforeStep(this)
    schedules foreach {
      case (t, route) =>
        l.onTrainMove(this, t, route.head)
    }
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