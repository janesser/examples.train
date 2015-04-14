package examples.train.passengers

import examples.train._
import examples.train.simulation._

class PassengerAnalyser extends Listener[PassengerEvent] {
  protected var totalWaitingTime: Time = 0

  protected var totalTravellingTime: Time = 0

  @throws[SimulationException]
  override def afterStep(event: PassengerEvent): Unit =
    event match {
      case PassengersOnTrain(time, p, t, waiting) =>
        totalWaitingTime += (p * waiting)
      case PassengersOffTrain(time, p, s2, travelling) =>
        totalTravellingTime += (p * travelling)
    }

  def score: Time = totalTravellingTime - totalWaitingTime
}
