package scenario.testing

/** Run all measurements at once. */
object MeasureAll {
  def main(args: Array[String]): Unit = {
    MeasureStaticAssignment.main(args)
    MeasureLunchVariants.main(args)
    MeasureVariables.main(args)
    MeasureBadSolverLikelihood.main(args)
    MeasureVaryingTimeLimit.main(args)
    MeasureLunchSimulator.main(args)
  }
}
