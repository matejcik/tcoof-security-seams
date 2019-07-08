package tcof

import org.chocosolver.solver.variables.BoolVar
import tcof.InitStages.InitStages

trait WithSelectionStatus extends Initializable {
  private[tcof] var isSelectedVar: BoolVar = _

  def selected: Boolean = _solverModel.solution.getIntVal(isSelectedVar) == 1

  override private[tcof] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)

    stage match {
      case InitStages.VarsCreation =>
        isSelectedVar = _solverModel.boolVar()
      case _ =>
    }
  }
}
