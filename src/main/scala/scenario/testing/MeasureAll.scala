package scenario.testing

object MeasureAll {
  def main(args: Array[String]): Unit = {
    MeasureLunchVariants.main(args)
    MeasureVariables.main(args)
    MeasureBadSolverLikelihood.main(args)
    MeasureVaryingTimeLimit.main(args)
    MeasureLunchSimulator.main(args)
  }
}
