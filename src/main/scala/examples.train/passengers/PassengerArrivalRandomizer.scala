package examples.train.passengers

import examples.train._

case class PassengerArrivalRandomizer(maxRandomArrivals: Int = 3,
                                      maxRandomPassengers: PassengerCount = 256,
                                      r: java.util.Random = new java.util.Random(1L))
                                     (implicit override val network: Seq[Railway]) extends NetworkAnalyser {

  protected def randomPassengerCount(waiting: PassengerCount = maxRandomPassengers): PassengerCount =
    r.nextInt(waiting)

  def randomStation(stations: Seq[Station] = super.stations): Station =
    stations.toList(r.nextInt(stations.size))


  /**
   * @param timeInterval [t1, t2]
   * @return t1 + ([0,1] * t2)
   */
  def randomTime(timeInterval: (Time, Time)): Time =
    timeInterval._1 + (BigDecimal(r.nextDouble()).abs * timeInterval._2)

  def simulateArrivals(s1: Station, timeInterval: (Time, Time)): Seq[PassengerArrival] =
    Range.inclusive(0, 1 + r.nextInt(maxRandomArrivals)) map { i =>
      // new waiting passengers with random destinations
      PassengerArrival(
        s1,
        randomStation(super.stations.filter(_ != s1)),
        randomPassengerCount(),
        randomTime(timeInterval))
    } sortBy (_.time)
}
