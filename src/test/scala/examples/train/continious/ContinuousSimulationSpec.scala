package examples.train.continious

import examples.train._
import examples.train.passengers._
import examples.train.simulation._

class ContinuousSimulationSpec extends PassengerSimulatorSpec {

  object EmptyContiniousScheduler extends ContinuousScheduler[PassengerEvent, PassengerTrain] {
    override def schedule() = Map()
  }

  case class OneTimeContiniousScheduler(override val network: Seq[Railway]) extends ContinuousScheduler[PassengerEvent, PassengerTrain] with NetworkAnalyser {
    var rescheduled = false

    override def schedule() = if (!rescheduled) Map(t() -> network.toStream) else Map()
  }

  case class OnDemandScheduler(override val network: Seq[Railway]) extends ContinuousScheduler[PassengerEvent, PassengerTrain] with NetworkAnalyser {
    val trainsRequested = scala.collection.mutable.HashMap.empty[PassengerTrain, Stream[Railway]]

    @throws[SimulationException]
    override def beforeStep(event: PassengerEvent): Unit =
      event match {
        case PassengerArrival(s1, s2, p, time) =>
          super.findRoute(s1, s2) match {
            case Some(route) =>
              trainsRequested += t() -> route.toStream
            case None => // ignore unreachable
          }
      }

    override def schedule() = {
      val newSchedule = trainsRequested.toMap
      trainsRequested.clear()
      newSchedule
    }
  }

  "ContinuousSimulationSpec" should "simulate pre-scheduled routes" in withPassengerSimulator { (r, network, s) =>
    ContinuousSimulation(EmptyContiniousScheduler, s).simulate(Map(t() -> network)) should not be empty
  }

  it should "simulate just-in-time scheduled trains" in withPassengerSimulator { (r, network, s) =>
    val trains = ContinuousSimulation(OneTimeContiniousScheduler(network), s).simulate(Map(t() -> network)) filter {
      case arr: PassengersOnTrain => true
      case _ => false
    } map {
      case PassengersOnTrain(time, p, t, waitingSince) => t
    } toSet

    trains should have size 2
  }

  it should "simulate continiously" in withPassengerSimulator { (r, network, s) =>
    val trains = ContinuousSimulation(OnDemandScheduler(network), s).simulate(Map(t() -> network)) filter {
      case arr: PassengersOnTrain => true
      case _ => false
    } map {
      case PassengersOnTrain(time, p, t, waitingSince) => t
    } toSet

    trains.take(3) should have size 3
  }

  "ContinuousAnalyser" should "measure travel- and waiting-times relative to elapsed time" in {
    val train = t()
    val s2 = stations().head
    val analyser = new ContinuousAnalyser
    analyser.afterStep(PassengersOnTrain(0, 10, train, 0.5))
    analyser.afterStep(PassengersOffTrain(1.0, 5, s2, 0.5))
    analyser.afterStep(PassengersOnTrain(2.0, 2, train, 0.5))
    analyser.score shouldEqual ((10 * -0.5) + (5 * 0.5) + (2 * -0.5)) / 2.0
  }
}
