package tcof

import org.chocosolver.solver.Model

class Scenario[EnsembleType <: Ensemble](builder: () => EnsembleType) {
  private var _solution: EnsembleType = _
  private var _hasUtility = false
  private var _actions: Iterable[Action] = List()

  private val _solverModel = new SolverModel

  def instance: EnsembleType = _solution

  def init(): Unit = {
    _solution = builder()
    val config = new Config(_solverModel)

    // initialize solution
    for (stage <- InitStages.values) {
      _solution._init(stage, config)
    }

    // initialize root component
    _solverModel.post(_solution._buildConstraintsClause)

    // configure utility
    _solution._collectUtility match {
      case Some(_solverModel.IntegerIntVar(utility)) =>
        _hasUtility = true
        _solverModel.setObjective(Model.MAXIMIZE, utility)
      case _ =>
        _hasUtility = false
    }

    config.solverModel.init()
  }

  def solverLimitTime(limit: Long) =
    _solverModel.getSolver.limitTime(limit)

  def solve(): Boolean = _solverModel.solveAndRecord()

  def exists: Boolean = _solverModel.exists


  def commit(): Unit = {
    _actions = instance._collectActions()
  }

  def actions: Iterable[Action] = _actions

  def resolve(): Boolean = {
    init()
    if (_hasUtility) {
      // repeatedly solve to optimize objective
      while (solve()) {}
    } else {
      // solve and record first solution
      solve()
    }
    if (exists) commit()
    return exists
  }
}

object Scenario {
  def root[E <: Ensemble](builder: => E): Scenario[E] =
    new Scenario(builder _)
}
