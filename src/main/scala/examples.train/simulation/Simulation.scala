package examples.train.simulation

import examples.train._

trait Simulator[S <: State] {

  import scala.collection.mutable

  def simulate(schedules: Map[Train, Seq[Railway]]): Stream[S]

  protected implicit var listeners: mutable.Seq[Listener[S]] = mutable.Seq()

  def register(l: Listener[S]): Unit =
    synchronized {
      listeners = listeners ++ Seq(l)
    }
}

trait State

class SimulationException extends Exception

trait Listener[-S <: State] {
  @throws[SimulationException]
  def onTrainMove(state: S, t: Train, r: Railway): Unit = {}

  @throws[SimulationException]
  def beforeStep(state: S): Unit = {}
}