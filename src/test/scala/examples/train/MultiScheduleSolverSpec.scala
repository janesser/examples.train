package examples.train

class MultiScheduleSolverSpec extends SimpleScheduleSolverSpec {

  override def solver(network: Seq[Railway]): ScheduleSolver =
    new MultiScheduleSolver(network)

  "MultiScheduleSolver" should "optimize linear with multiple trains" in {
    super.solveAndMeasure(linear(4), trains(2), DEFAULT_DISTANCE / DEFAULT_SPEED)
  }
}
