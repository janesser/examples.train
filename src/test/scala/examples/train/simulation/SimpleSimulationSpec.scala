package examples.train.simulation

import examples.train._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Suite, FlatSpec, Matchers}

trait ListenerTest {
  this: Suite with MockFactory with Networks with Trains =>

  def validateListenerNotification[S <: State](s: Simulator[S]): Unit = {
    val l = mock[Listener[S]]

    (l.beforeStep _).expects(*).repeat(2)
    (l.onTrainMove _).expects(*, *, *).repeat(2)
    (l.afterStep _).expects(*).repeat(2)

    s.register(l)
    s.simulate(Map(t() -> linear(3)._2)).force
  }
}

class SimpleSimulationSpec
  extends FlatSpec
  with Matchers
  with MockFactory
  with ListenerTest
  with Networks
  with Trains {

  def withSimpleSimulator(testCode: SimpleSimulator => Any): Unit = {
    val simulator = new SimpleSimulator
    testCode(simulator)
  }

  def withSimpleStateSteps(testCode: Seq[SimpleState] => Any): Unit = {
    val simulator = new SimpleSimulator
    val simulation = simulator.simulate(Map(t() -> linear(3)._2))
    testCode(simulation.takeWhile(_.hasMore))
  }

  "SimpleSimulator" should "simulate linear" in withSimpleStateSteps { steps =>
    steps should have size 2

    val first = steps.head
    first.trains should have size 1
  }

  it should "notify listeners" in withSimpleSimulator(validateListenerNotification)

  "CollisionDetector" should "be informed" in withSimpleSimulator { s =>
    s.register(CollisionDetector)

    // same route for two trains
    val route = linear(2)._2

    an[CollisionDetector.CollisionException] should be thrownBy
      s.simulate(Map(t() -> route, t() -> route))
  }

  "SimulationAnalyser" should "record all trains working" in withSimpleSimulator { s =>
    val analyser = new SimulationAnalyser
    s.register(analyser)

    s.simulate(Map(t() -> linear(3)._2)).takeWhile(_.hasMore) should have size 2

    analyser.work should have size 1
    analyser.score shouldBe 2 * DEFAULT_DISTANCE / DEFAULT_SPEED
  }
}
