package examples.train

import org.scalatest.{Matchers, FlatSpec}

class ScheduleAnalyserSpec extends FlatSpec with Matchers with Networks with Trains {

  "ScheduleAnalyser" should "measure one way route" in
    new ScheduleAnalyserTest(Map(t() -> linear(2)._2), DEFAULT_DISTANCE / DEFAULT_SPEED)

  it should "measure linear route" in
    new ScheduleAnalyserTest(Map(t() -> linear(3)._2), 2 * DEFAULT_DISTANCE / DEFAULT_SPEED)

  it should "measure with route-less train" in
    new ScheduleAnalyserTest(Map(t() -> linear(2)._2, t() -> Seq()), DEFAULT_DISTANCE / DEFAULT_SPEED)

  it should "detect incomplete coverage" in {
    an[IllegalArgumentException] should be thrownBy
      new ScheduleAnalyserTest(Map(t() -> linear(3)._2), DEFAULT_DISTANCE / DEFAULT_SPEED) {
        override def network = super.network ++ linear(2)._2
      }
  }
}

case class ScheduleAnalyserTest(schedule: Map[Train, Seq[Railway]], expected: Time)
  extends ScheduleAnalyser
  with NetworkAnalyser
  with Matchers {

  override def network = schedule.values.flatten.toSeq

  def stations = network.map(_.s1) ++ network.map(_.s2)

  measure(stations, schedule) shouldBe expected
}