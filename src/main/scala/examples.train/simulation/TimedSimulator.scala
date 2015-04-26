package examples.train.simulation

import examples.train._

import scala.collection.immutable.Iterable

case class ArrivalEvent(override val time: Time, t: Train, r: Railway) extends Event

case class TimedSimulator(override val lookAhead: Int = 4) extends TimedSimulation[ArrivalEvent, Train] {
  def simulateTime(arrivalsByTime: Seq[(Time, Train, Seq[Railway])]): Stream[ArrivalEvent] =
    arrivalsByTime.headOption match {
      case Some(arrival) =>
        val e = ArrivalEvent(arrival._1, arrival._2, arrival._3.head)
        listeners foreach { l =>
          l.beforeStep(e)
          l.onTrainMove(e, e.t, e.r)
          l.afterStep(e)
        }
        e #:: simulateTime(arrivalsByTime.tail)
      case None =>
        Stream.Empty
    }

}