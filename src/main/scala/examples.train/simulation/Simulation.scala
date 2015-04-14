package examples.train.simulation

import examples.train._

trait Simulator[S <: State, T <: Train] {

  import scala.collection.mutable

  def simulate(schedules: Map[T, Seq[Railway]]): Stream[S]

  protected implicit val listeners: mutable.Buffer[Listener[S]] = mutable.Buffer()

  def register(l: Listener[S]): Unit =
    synchronized {
      listeners += l
    }
}

trait State

class SimulationException extends Exception

trait Listener[-S <: State] {
  @throws[SimulationException]
  def onTrainMove(state: S, t: Train, r: Railway): Unit = {}

  @throws[SimulationException]
  def beforeStep(state: S): Unit = {}

  @throws[SimulationException]
  def afterStep(state:S):Unit = {}
}