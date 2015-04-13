package examples.train.simulation

import examples.train._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

class TimedSimulatorSpec
  extends FlatSpec
  with Matchers
  with MockFactory
  with ListenerTest
  with Trains
  with Networks {

  def withTimedSimulator(testCode: TimedSimulator => Any): Unit = {
    testCode(new TimedSimulator)
  }

  "TimedSimulator" should "simulate linear events" in withTimedSimulator { s =>
    val train = t()
    val route = linear(3)._2

    val events = s.simulate(Map(train -> route)).toSeq
    events should have size 2

    val first = events.head
    first.time shouldBe time(t.spd, route.head.d)
    first.t shouldBe train
    first.r shouldBe route.head

    val second = events.drop(1).head
    second.r shouldBe route.drop(1).head
  }

  it should "simulate multiple linear events" in withTimedSimulator { s =>
    val route = linear(5)._2
    val events = s.simulate(
      Map(
        Train("theSlow", BigDecimal(0.5)) -> route,
        Train("theFast", BigDecimal(2)) -> route
      ))
    events should have size 8

    events.take(4) foreach {
      e =>
        e.t.id shouldBe "theFast"
    }

    events.drop(4) foreach {
      e =>
        e.t.id shouldBe "theSlow"
    }
  }

  it should "simulate multiple random events" in withTimedSimulator { s =>
    val route = linear(5)._2
    val events = s.simulate(
      Map(
        Train("A", BigDecimal(0.33)) -> route,
        Train("B", BigDecimal(0.66)) -> route,
        Train("C", BigDecimal(0.99)) -> route
      ))
    events should have size (4 * 3)

    def eventsOf(traindId: String) =
      events filter {
        e => e.t.id == "A"
      }

    val eventsA = eventsOf("A")
    eventsA should have size 4
    eventsA.last.time shouldBe (time(BigDecimal(0.33), BigDecimal(4)))
  }

  it should "notify listeners" in withTimedSimulator(validateListenerNotification)
}
