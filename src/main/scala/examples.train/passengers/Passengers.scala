package examples.train.passengers

import examples.train._
import examples.train.simulation._


object `package` {
  type PassengerCount = Int
}

case class PassengerTrain(override val id: String, override val spd: Speed,
                          cap: PassengerCount) extends Train(id, spd)

trait PassengerEvent extends Event {
  def p: PassengerCount
}

case class PassengerArrival(s1: Station, s2: Station, override val p: PassengerCount, time: Time) extends PassengerEvent

case class PassengersOnTrain(override val time: Time, override val p: PassengerCount, t: Train, waiting: Time) extends PassengerEvent

case class PassengersOffTrain(override val time: Time, override val p: PassengerCount, s2: Station, travelling: Time) extends PassengerEvent

case class PassengerTrainArrival(override val time: Time, override val t: Train, override val r: Railway, override val p: PassengerCount)
  extends ArrivalEvent(time, t, r)
  with PassengerEvent




