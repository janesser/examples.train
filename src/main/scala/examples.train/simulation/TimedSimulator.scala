package examples.train.simulation

import examples.train._

case class Event(tr: Train, t: Time, r: Railway) extends State {
  def !(implicit listeners: Seq[Listener[Event]]): Unit =
    listeners foreach {
      _.onTrainMove(this, tr, r)
    }
}

class TimedSimulator extends Simulator[Event] {
  import scala.language.postfixOps
  
  override def simulate(schedules: Map[Train, Seq[Railway]]): Stream[Event] = {
    def timedSchedules: Stream[Event] = {
      val timed = schedules map {
        case (t, route) =>
          (t, time(route.head.d, t.spd), route)
      } toSeq

      timed sortBy {
        case (t, time, route) =>
          time
      } map {
        case (t, time, route) =>
          val e = Event(t, time, route.head)
          listeners foreach {
            _.beforeStep(e)
          }
          e
      } toStream
    }

    timedSchedules append simulate(
      schedules map {
        case (t, route) =>
          t -> route.tail
      })
  }
}
