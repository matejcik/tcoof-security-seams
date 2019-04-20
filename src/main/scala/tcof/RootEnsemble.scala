package tcof

import org.chocosolver.solver.Model
import tcof.InitStages.InitStages

class RootEnsemble extends Ensemble {
  name("<root>")

  private var _collectedUtility: Option[Integer] = None

  override private[tcof] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)

    stage match {
      case InitStages.RulesCreation =>
        val sm = _solverModel
        _collectedUtility = _collectUtility
        _collectedUtility match {
          case Some(sm.IntegerIntVar(utilityVar)) =>
            _solverModel.setObjective(Model.MAXIMIZE, utilityVar)
          case _ =>
        }

        _solverModel.post(_buildConstraintsClause)
      case _ =>
    }
  }

  override def solutionUtility: Int = _collectedUtility match {
    case Some(v) => v.solutionValue
    case _       => 0
  }
}
