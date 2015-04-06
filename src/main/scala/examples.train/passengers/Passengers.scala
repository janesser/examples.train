package examples.train.passengers

import examples.train._
import examples.train.simulation._

object `package` {
  type PassengerCount = Int
}

case class PassengerTrain(override val id: String, override val spd: Speed,
                          cap: PassengerCount) extends Train(id, spd)

case class Passengers(stations: Seq[Station],
                      initialPassengers: PassengerCount = 0,
                      maxNewPassengers: PassengerCount = 10,
                      random: java.util.Random = new java.util.Random())
  extends examples.train.simulation.Listener[State] {

  var passengers: Map[Station, PassengerCount] =
    Map.empty[Station, PassengerCount].withDefault(_ => initialPassengers)

  override def beforeStep(state: State): Unit =
    stations foreach {
      s =>
        // TODO add destination
        passengers += (s -> (passengers(s) + random.nextInt(maxNewPassengers)))
    }

  @throws[SimulationException]
  override def onTrainMove(state: State, t: Train, r: Railway): Unit = {
    val s1ps = passengers(r.s1)
    val trainCap = t.asInstanceOf[PassengerTrain].cap
    // assume destination reached
    passengers += (r.s1 -> (if (s1ps > trainCap) s1ps - trainCap else 0))

  }
}
