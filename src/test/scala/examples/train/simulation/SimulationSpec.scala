package examples.train.simulation

import examples.train.{Railway, Train}

class SimulationSpec {

  trait SpecialState extends State

  trait SimulatorState extends State

  trait EnhancedSimulatorState extends SimulatorState

  val s = new Simulator[EnhancedSimulatorState, Train] {
    override def simulate(schedules: Map[Train, Seq[Railway]]): Stream[EnhancedSimulatorState] = ???
  }
  s.register(new Listener[State] {})
  s.register(new Listener[SimulatorState] {})
  s.register(new Listener[EnhancedSimulatorState] {})
  // type error: s.register(new Listener[SpecialState] {})

}
