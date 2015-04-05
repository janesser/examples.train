package examples.train

import org.scalatest.{Matchers, FlatSpec}

trait Networks {
  val DEFAULT_DISTANCE: Distance = BigDecimal(1)

  def stations(numberOfStations: Int = 1): Seq[Station] =
    Range.inclusive(1, numberOfStations) map {
      i => Station(s"station #$i ${System.nanoTime()}")
    }

  def w(s1: Station, s2: Station): Railway = Railway(s1, s2, DEFAULT_DISTANCE)

  def linear(stations: Seq[Station]): Seq[Railway] = {
    require(stations.size >= 2)

    stations.tail.foldLeft(Seq.empty[Railway]) {
      (network, station) =>
        network :+ w(if (network.isEmpty) stations.head else network.last.s2, station)
    }
  }

  def linear(numberOfStations: Int = 3): (Seq[Station], Seq[Railway]) = {
    val s = stations(numberOfStations)
    (s, linear(s))
  }

  lazy val triangle: (Seq[Station], Seq[Railway]) = {
    val s = stations(3)
    val lin = linear(s)
    (s, w(s.last, s.head) +: lin)
  }

  lazy val cross: (Seq[Station], Seq[Railway]) = {
    val s = stations(5)
    val ctr = s(1)
    val lin = linear(s.take(3))
    (s, lin ++ Seq(w(s(3), ctr), w(s(4), ctr)))
  }
}

class NetworkAnalyserSpec extends FlatSpec with Matchers with Networks {

  case class NetworkAnalyserTest(network: Seq[Railway]) extends NetworkAnalyser

  "NetworkAnalyser" should "detect connected on one way" in
    new NetworkAnalyserTest(linear(2)._2) {
      isConnected() shouldBe true
    }

  it should "detect unconnected two ways" in
    new NetworkAnalyserTest(linear(2)._2 ++ linear(2)._2) {
      isConnected() shouldBe false
    }

  it should "not partition connected ways" in
    new NetworkAnalyserTest(linear()._2) {
      connectedPartitions() should have size 1
    }

  it should "partition unconnected ways" in
    new NetworkAnalyserTest(linear(2)._2 ++ linear(2)._2) {
      connectedPartitions() should have size 2
    }

  it should "find some way" in
    new NetworkAnalyserTest(linear(3)._2) {
      findRoute(network.head.s1, network.last.s2) shouldBe defined
    }

  it should "find no way" in
    new NetworkAnalyserTest(linear(2)._2 ++ linear(2)._2) {
      findRoute(network.head.s1, network.last.s2) should not be defined
    }

}
