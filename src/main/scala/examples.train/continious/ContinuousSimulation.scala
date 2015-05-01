package examples.train.continious

import examples.train._
import examples.train.passengers.{PassengerEvent, PassengerAnalyser}
import examples.train.simulation._

trait ContinuousScheduler[E <: Event, T <: Train] extends Listener[E] {
  def schedule(): Map[T, Stream[Railway]]
}

case class ContinuousSimulation[E <: Event, T <: Train](scheduler: ContinuousScheduler[E, T],
                                                        simulator: Simulator[E, T])
  extends Simulator[E, T] {

  register(scheduler)

  override def simulate(schedules: Map[T, Seq[Railway]]): Stream[E] =
    simulator.simulate(schedules) #::: simulator.simulate(scheduler.schedule())

  override def register(l: Listener[E]): Unit =
    simulator.register(l)
}

class ContinuousAnalyser extends PassengerAnalyser {

  var elapsedTime: Time = 0

  @throws[SimulationException] override
  def afterStep(event: PassengerEvent): Unit = {
    elapsedTime = event.time
    super.afterStep(event)
  }

  override def score: Time = super.score / elapsedTime
}