package examples.train.simulation

import examples.train._

trait Event extends State {
  def time: Time
}

trait TimedSimulation[E <: Event, T <: Train] extends Simulator[E, T] {

  import scala.language.{implicitConversions, postfixOps}

  def lookAhead: Int

  implicit def timedRouteFlatten(timedTrainRoutes: Iterable[Seq[(Time, T, Seq[Railway])]]): Seq[(Time, T, Seq[Railway])] =
    timedTrainRoutes.toSeq.flatten

  override def simulate(schedules: Map[T, Seq[Railway]]): Stream[E] =
    if (schedules.isEmpty) Stream.Empty
    else simulateTime(schedules map {
      case (t, route) =>
        route.take(lookAhead).foldLeft((BigDecimal(0), Seq.empty[(Time, T, Seq[Railway])])) {
          (acc, r) =>
            val arrivalTime = acc._1 + time(t.spd, r.d)
            (arrivalTime, acc._2 :+(arrivalTime, t, route.dropWhile(_ != r)))
        }._2
    } sortBy {
      // timedRouteFlatten
      case (time, t, route) =>
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

  def simulateTime(arrivalsByTime: Seq[(Time, T, Seq[Railway])]): Stream[E]
}
