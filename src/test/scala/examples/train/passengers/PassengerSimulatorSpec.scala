package examples.train.passengers

import examples.train._
import examples.train.simulation.ListenerTest
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

trait PassengerTrains extends Trains {
  val DEFAULT_CAPACITY: PassengerCount = 100

  implicit override def t(): PassengerTrain =
    PassengerTrain(java.util.UUID.randomUUID().toString, DEFAULT_SPEED, DEFAULT_CAPACITY)
}

class PassengerSimulatorSpec
  extends FlatSpec
  with Matchers
  with MockFactory
  with ListenerTest[PassengerEvent, PassengerTrain]
  with PassengerTrains
  with Networks {

  def withPassengerRandomizer(testCode: (PassengerArrivalRandomizer, Seq[Railway]) => Any): Unit = {
    implicit val network = linear(5)._2

    // fixed randomness
    testCode(PassengerArrivalRandomizer(), network)
  }

  "RandomPassengerArrivals" should "emit passenger arrival events" in withPassengerRandomizer { (r, network) =>
    val timeInterval = (BigDecimal(0), BigDecimal(1))
    val arrivals = r.simulateArrivals(network.head.s1, timeInterval)

    arrivals should not be empty

    all(arrivals.map(_.time)) should (be >= timeInterval._1 and be <= timeInterval._2)

    all(arrivals map { a =>
      a.s1 != a.s2
    }) shouldEqual true

    // based on pseudo random seed 1L
    arrivals.head should have(
      'p(104),
      's1(network(0).s1),
      's2(network(1).s2)
    )
  }

  def withPassengerSimulator(testCode: (PassengerArrivalRandomizer, Seq[Railway], PassengerSimulator) => Any): Unit = {
    withPassengerRandomizer { (r, network) =>
      testCode(r, network, PassengerSimulator()(network, r))
    }
  }

  "PassengerTracking" should "reduce free places on train" in {
    new PassengerTracking {
      val cap = 10
      val p = 6
      val time: Time = 0.1

      val t = PassengerTrain("testPassengerTrain", DEFAULT_SPEED, cap)
      val s2 = stations().head

      adjustOnTrain(t, s2, time)(_ + p)
      passengersOnTrain(t) shouldEqual p

      toDestination(t, s2) match {
        case Some(travellers) =>
          travellers should have size 1
          val traveller = travellers.head
          traveller._1 shouldEqual time
          traveller._2 shouldEqual p
        case None => fail()
      }
      freeOnTrain(t) shouldEqual cap
    }
  }

  /**
   * Train meets passengers, based on random seed set at [[PassengerSimulatorSpec.withPassengerRandomizer()]]
   */
  "PassengerSimulator" should "simulate passenger events" in withPassengerSimulator { (r, network, s) =>
    val boardings = s.simulate(Map(t() -> network)).force

    boardings.filter(_.isInstanceOf[PassengersOnTrain]) should not be empty
    boardings.filter(_.isInstanceOf[PassengersOffTrain]) should not be empty
  }

  /**
   * Expectations based on random seed set at [[PassengerSimulatorSpec.withPassengerRandomizer()]]
   */
  override val expectedBeforeCount = 10
  // PassengerArrival
  override val expectedTrainMoveCount = 5 - 1
  // PassengerTrainArrival
  override val expectedAfterCount = 5 // PassengersOnTrain | PassengersOffTrain
  it should "notify listeners" in withPassengerSimulator { (r, network, s) =>
    validateListenerNotification(s)(t, network)
  }

  "PassengerAnalyser" should "measure travel times" in {
    val s = stations(1).head

    val l = new PassengerAnalyser
    l.afterStep(PassengersOnTrain(0.0, 10, t(), 1.0))
    l.afterStep(PassengersOffTrain(0.5, 5, s, 0.5))
    l.score shouldEqual ((5 * 0.5) - (10 * 1.0))
  }
}
