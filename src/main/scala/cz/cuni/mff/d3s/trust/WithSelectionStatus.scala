package cz.cuni.mff.d3s.trust

import org.chocosolver.solver.variables.BoolVar
import InitStages.InitStages

/** Ensemble part for handling ensemble selection status.
  *
  * Does not provide DSL functions. It is only useful internally, as a dependency of
  * traits that need to use the selection status.
  */
trait WithSelectionStatus extends Initializable {
  /** Solver variable representing selection status of this ensemble. */
  private[trust] var isSelectedVar: BoolVar = _

  /** True if the ensemble is selected in the solution.
    *
    * Can only be used when a solution exists.
    */
  def selected: Boolean = _solverModel.solution.getIntVal(isSelectedVar) == 1

  /** Initialization hook.
    *
    * Ensures that the selection status variable is created.
    */
  override private[trust] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)

    stage match {
      case InitStages.VarsCreation =>
        isSelectedVar = _solverModel.boolVar()
      case _ =>
    }
  }
}
