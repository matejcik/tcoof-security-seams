package cz.cuni.mff.d3s.enact

import org.chocosolver.solver.variables.BoolVar
import InitStages.InitStages

trait WithSelectionStatus extends Initializable {
  private[enact] var isSelectedVar: BoolVar = _

  def selected: Boolean = _solverModel.solution.getIntVal(isSelectedVar) == 1

  override private[enact] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)

    stage match {
      case InitStages.VarsCreation =>
        isSelectedVar = _solverModel.boolVar()
      case _ =>
    }
  }
}
