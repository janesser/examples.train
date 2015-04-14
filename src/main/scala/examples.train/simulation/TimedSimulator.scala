package examples.train.simulation

import examples.train._

import scala.collection.immutable.Iterable

case class Event(time: Time, t: Train, r: Railway) extends State

case class TimedSimulator(lookAhead: Int = 4) extends Simulator[Event] {

  import scala.language.postfixOps

  implicit def timedRouteFlatten(timedTrainRoutes: Iterable[Seq[(Time, Train, Railway)]]): Seq[(Time, Train, Railway)] =
    timedTrainRoutes.toSeq.flatten

  override def simulate(schedules: Map[Train, Seq[Railway]]): Stream[Event] =
    if (schedules.isEmpty) Stream.Empty
    else simulateTime(schedules map {
      case (t, route) =>
        route.take(lookAhead).foldLeft((BigDecimal(0), Seq.empty[(Time, Train, Railway)])) {
          (acc, r) =>
            val arrivalTime = acc._1 + time(t.spd, r.d)
            (arrivalTime, acc._2 :+(arrivalTime, t, r))
        }._2
    } sortBy { // timedRouteFlatten
      case (time, t, r) =>
        /*
        sort by
        1) time
        2) train id
         */
        (time, t.id)
    }) #::: simulate(schedules map {
      case (t, route) =>
        t -> route.drop(lookAhead)
    } filter {
      case (t, route) =>
        route.nonEmpty
    })

  def simulateTime(arrivalsByTime: Seq[(Time, Train, Railway)]): Stream[Event] =
    arrivalsByTime.headOption match {
      case Some(arrival) =>
        val e = Event(arrival._1, arrival._2, arrival._3)
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
