package tcof

import tcof.InitStages.InitStages
import org.chocosolver.solver.Model

class RootEnsemble extends Ensemble {
  name("<root>")

  override private[tcof] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)

    stage match {
      case InitStages.RulesCreation =>
        val sm = _solverModel
        _getUtility match {
          case Some(sm.IntegerIntVar(utilityVar)) =>
            _solverModel.setObjective(Model.MAXIMIZE, utilityVar)
          case _ =>
        }

        _solverModel.post(_buildConstraintsClause)
      case _ =>
    }
  }
}
