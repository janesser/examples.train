package examples.train

import org.scalatest.{Matchers, FlatSpec}

class SimulationSpec extends FlatSpec with Matchers with Networks with Trains {

  "Simulator" should "simulate linear" in {
    val simTrains = trains(1)

    val simulation = new Simulator().simulate(
      new SimpleScheduleSolver(linear(3)._2).solve(simTrains))
    val steps = simulation.takeWhile(_.hasMore)
    steps should have size 2

    val first = steps.head
    first.trains.toSeq shouldEqual simTrains
  }

  it should "inform listeners about movings" in {
    val simulator = new Simulator
    simulator.listeners += CollisionDetector

    val route = linear(2)._2
    val simulation = simulator.simulate(Map(t() -> route, t() -> route)).head
    simulation.trains should have size 2

    an[CollisionDetector.CollisionException] should be thrownBy
      simulation.step()
  }
}
