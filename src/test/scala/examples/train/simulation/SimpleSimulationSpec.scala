package examples.train.simulation

import org.scalatest.{FlatSpec, Matchers}
import examples.train._

class SimpleSimulationSpec extends FlatSpec with Matchers with Networks with Trains {

  "SimpleSimulator" should "simulate linear" in {
    val simTrains = trains(1)

    val simulation = new SimpleSimulator().simulate(
      new SimpleScheduleSolver(linear(3)._2).solve(simTrains))
    val steps = simulation.takeWhile(_.hasMore)
    steps should have size 2

    val first = steps.head
    first.trains.toSeq shouldEqual simTrains
  }

  it should "inform listeners about movings" in {
    val simulator = new SimpleSimulator
    simulator.listeners += CollisionDetector

    val route = linear(2)._2
    val simulation = simulator.simulate(Map(t() -> route, t() -> route)).head
    simulation.trains should have size 2

    an[CollisionDetector.CollisionException] should be thrownBy
      simulation.step()
  }

  "SimulationAnalyser" should "record all trains working" in {
    val simulator = new SimpleSimulator
    val analyser = new SimulationAnalyser[SimpleState]
    simulator.listeners += analyser

    simulator.simulate(Map(t() -> linear(3)._2)).takeWhile(_.hasMore) should have size 2

    analyser.work should have size 1

    analyser.score shouldBe 2 * DEFAULT_DISTANCE / DEFAULT_SPEED
  }
}
