package cz.cuni.mff.d3s.trust

import org.chocosolver.solver.variables.BoolVar
import InitStages.InitStages

trait WithSelectionStatus extends Initializable {
  private[trust] var isSelectedVar: BoolVar = _

  def selected: Boolean = _solverModel.solution.getIntVal(isSelectedVar) == 1

  override private[trust] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)

    stage match {
      case InitStages.VarsCreation =>
        isSelectedVar = _solverModel.boolVar()
      case _ =>
    }
  }
}
