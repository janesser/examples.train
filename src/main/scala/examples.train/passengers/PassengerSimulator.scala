package examples.train.passengers

import examples.train._
import examples.train.simulation._

trait PassengerTracking {

  import scala.language.implicitConversions

  import scala.collection.mutable

  implicit val passengerCountDefault: PassengerCount = 0

  implicit class AdjustableMap[K, V](m: scala.collection.mutable.Map[K, V])(implicit default: V) {
    def adjust(k: K)(f: V => V) = m.update(k, f(m.getOrElseUpdate(k, default)))
  }

  private val passengersByDestination: mutable.Map[PassengerTrain, mutable.Map[Station, mutable.Map[Time, PassengerCount]]] = mutable.HashMap()
  private val passengersByStation: mutable.Map[Station, mutable.Map[Station, mutable.Map[Time, PassengerCount]]] = mutable.HashMap()

  def adjustOnTrain(t: PassengerTrain, s2: Station, time: Time)(f: PassengerCount => PassengerCount): Unit =
    passengersByDestination
      .getOrElseUpdate(t, mutable.HashMap())
      .getOrElseUpdate(s2, mutable.HashMap())
      .adjust(time)(f)

  def adjustAtStation(s1: Station, s2: Station, time: Time)(f: PassengerCount => PassengerCount): Unit =
    passengersByStation
      .getOrElseUpdate(s1, mutable.HashMap())
      .getOrElseUpdate(s2, mutable.HashMap())
      .adjust(time)(f)

  def passengersOnTrain(t: PassengerTrain): PassengerCount =
    passengersByDestination.getOrElse(t, mutable.HashMap()).foldLeft(0) {
      case (acc, byDestination) =>
        acc + byDestination._2.foldLeft(0) {
          case (acc, byDepartureTime) =>
            acc + byDepartureTime._2
        }
    }

  def freeOnTrain(t: PassengerTrain): PassengerCount =
    t.cap - passengersOnTrain(t)

  def toDestination(t: PassengerTrain, s2: Station): Option[mutable.Map[Time, PassengerCount]] =
    passengersByDestination
      .getOrElse(t, mutable.HashMap())
      .remove(s2)

  def forDestinations(s1: Station): mutable.Iterable[(Station, Time, PassengerCount)] =
    passengersByStation
      .getOrElse(s1, mutable.HashMap()) flatMap {
      case (s2, byArrivalTime) =>
        byArrivalTime map {
          case (time, p) =>
            (s2, time, p)
        }
    }

  def fromOrigin(s1: Station, s2: Station): Option[mutable.Map[Time, PassengerCount]] =
    passengersByStation
      .getOrElse(s1, mutable.HashMap())
      .remove(s2)
}

trait PassengerRouting {
  self: PassengerTracking with NetworkAnalyser =>

  def routePassenger(time: Time, s1: Station, s2: Station, p: PassengerCount, waitingSince: Time, t: PassengerTrain, route: Seq[Railway]): Option[PassengerEvent] =
    self.findRoute(s1, s2) match {
      case Some(passengerRoute) =>
        if (route.intersect(passengerRoute).nonEmpty) {
          val transported = scala.math.min(p, freeOnTrain(t))
          if (transported > 0) {
            adjustAtStation(s1, s2, time)(_ - transported)
            adjustOnTrain(t, s2, time)(_ + transported)
            Some(PassengersOnTrain(
              time,
              transported,
              t,
              time - waitingSince))
          } else None // train is full
        } else None // wrong train
      case None => None // unreachable destination
    }

  def destinationReached(time: Time, t: PassengerTrain, s2: Station): Seq[PassengerEvent] =
    toDestination(t, s2) match {
      // TODO passengers off to take another train
      case Some(travellers) =>
        travellers map {
          case (departureTime, p) =>
            PassengersOffTrain(time, p, s2, time - departureTime)
        } toSeq
      case None => Seq()
    }

  def boarding(time: Time, t: PassengerTrain, s1: Station, route: Seq[Railway]): Seq[PassengerEvent] =
    forDestinations(s1) flatMap {
      case (s2, waitingSince, p) =>
        routePassenger(time, s1, s2, p, waitingSince, t, route)
    } toSeq

}

case class PassengerSimulator(override val lookAhead: Int = 5)(implicit override val network: Seq[Railway], randomPassenders: PassengerArrivalRandomizer)
  extends TimedSimulation[PassengerEvent, PassengerTrain]
  with NetworkAnalyser
  with PassengerTracking
  with PassengerRouting {

  def simulateOffBoarding(time: Time, t: PassengerTrain, s: Station): Seq[PassengerEvent] =
    destinationReached(time, t, s)

  def simulateOnBoarding(time: Time, t: PassengerTrain, route: Seq[Railway], s: Station, passengerArrivals: Seq[PassengerArrival]): Seq[PassengerEvent] = {
    val justArrived: Seq[PassengerEvent] =
      passengerArrivals flatMap {
        case PassengerArrival(s1, s2, p, arrivalTime) =>
          if (s1 == s) {
            routePassenger(time, s1, s2, p, arrivalTime, t, route) match {
              case Some(e) =>
                Some(e)
              case None =>
                adjustAtStation(s1, s2, time)(_ + p)
                None
            }
          } else {
            adjustAtStation(s1, s2, time)(_ + p)
            None
          }
      }

    val beenWaiting: Seq[PassengerEvent] = boarding(time, t, s, route)

    justArrived ++ beenWaiting
  }

  override def simulateTime(arrivalsByTime: Seq[(Time, PassengerTrain, Seq[Railway])]): Stream[PassengerEvent] = {
    arrivalsByTime.headOption match {
      case Some(arrival) =>
        arrival match {
          case (arrivalTime, t, route) =>
            val r = route.head
            val departureTime = arrivalTime - time(t.spd, r.d)

            // TODO distribution of passenger arrivals at all stations?
            val passengerArrivals = randomPassenders.simulateArrivals(r.s2, (departureTime, arrivalTime))
            passengerArrivals foreach { e =>
              listeners foreach {
                _.beforeStep(e)
              }
            }

            val trainMove = PassengerTrainArrival(departureTime, t, r, passengersOnTrain(t))
            listeners foreach {
              _.onTrainMove(trainMove, t, r)
            }

            val offBoardings: Seq[PassengerEvent] = simulateOffBoarding(arrivalTime, t, r.s2)
            val onBoardings: Seq[PassengerEvent] = simulateOnBoarding(arrivalTime, t, route.tail, r.s2, passengerArrivals)

            val boardings = (offBoardings ++ onBoardings)
            boardings foreach { e =>
              listeners foreach {
                _.afterStep(e)
              }
            }

            boardings.toStream #::: simulateTime(arrivalsByTime.tail)
        }
      case None =>
        Stream.empty
    }
  }
}
