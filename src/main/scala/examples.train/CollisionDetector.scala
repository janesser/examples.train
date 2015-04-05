package examples.train

object CollisionDetector extends SimulationListener {

  case class CollisionException(r: Railway, second: Train, first: Train) extends SimulationException

  override def onTrainMove(state: SimulationState,
                           second: Train,
                           r: Railway): Unit = {
    state.trains.foreach(
      first => state.locate(first) match {
        case Some(way) =>
          if (second != first && way == r)
            throw new CollisionException(r, second, first)
        case None => // ignore
      })
  }
}
