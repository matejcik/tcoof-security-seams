package tcof

import tcof.InitStages.InitStages
import org.chocosolver.solver.Model

class RootEnsemble extends Ensemble {
  name("<root>")

  private var _collectedUtility : Option[Integer] = None

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

  def printUtility: Unit = {
    _collectedUtility match {
      case Some(v: Integer) => println(s"Solution utility: ${v.solutionValue}")
      case _ => println("No utility defined")
    }
  }
}
