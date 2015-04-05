package examples.train

import org.scalatest.{Matchers, FlatSpec}

trait Trains {
  val DEFAULT_SPEED: Speed = 1

  def t() = Train(java.util.UUID.randomUUID().toString, DEFAULT_SPEED)

  def trains(numberOfTrains: Int = 1): Seq[Train] = {
    require(numberOfTrains > 0)

    Range.inclusive(1, numberOfTrains) map {
      i => Train(s"train #$i", DEFAULT_SPEED)
    }
  }
}

class SimpleScheduleSolverSpec extends FlatSpec with Matchers with Networks with Trains {

  def solver(network: Seq[Railway]): ScheduleSolver = new SimpleScheduleSolver(network)

  def solveAndMeasure(net: (Seq[Station], Seq[Railway]), trains: Seq[Train], expectedMeasure: Time): Unit =
    new ScheduleAnalyserTest(solver(net._2).solve(trains), expectedMeasure)


  "SimpleScheduleSolver" should "solve linear route" in
    solveAndMeasure(linear(3), trains(1), 2 * DEFAULT_DISTANCE / DEFAULT_SPEED)

  it should "solve triangle" in
    solveAndMeasure(triangle, trains(1), 3 * DEFAULT_DISTANCE / DEFAULT_SPEED)

  it should "solve independent train routes" in {
    val net1 = linear(2)
    val net2 = linear(2)
    solveAndMeasure((net1._1 ++ net2._1, net1._2 ++ net2._2), trains(2), DEFAULT_DISTANCE / DEFAULT_SPEED)
  }

  it should "solve crossroads" in
    solveAndMeasure(cross, trains(1), 6 * DEFAULT_DISTANCE / DEFAULT_SPEED)
}