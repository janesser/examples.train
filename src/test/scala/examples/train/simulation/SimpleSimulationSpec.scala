package examples.train.simulation

import examples.train._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

class SimpleSimulationSpec
  extends FlatSpec
  with Matchers
  with MockFactory
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

  it should "notify listeners" in withSimpleSimulator { s =>
    val l = mock[Listener[SimpleState]]

    (l.beforeStep _).expects(*).repeat(3)
    (l.onTrainMove _).expects(*, *, *).repeat(2)

    s.register(l)
    s.simulate(Map(t() -> linear(3)._2)).takeWhile(_.hasMore) should have size 2
  }

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
