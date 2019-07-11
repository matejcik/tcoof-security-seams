package cz.cuni.mff.d3s.enact

import org.chocosolver.solver.Model

class Policy[EnsembleType <: Ensemble](builder: () => EnsembleType) {
  private var _solution: EnsembleType = _
  private var _utility: Option[Integer] = None
  private var _actions: Iterable[Action] = null

  private var _solverModel: SolverModel = _

  def init(): Unit = {
    _actions = null
    _solution = builder()
    _solverModel = new SolverModel
    val config = new Config(_solverModel)

    // initialize solution
    for (stage <- InitStages.values) {
      _solution._init(stage, config)
    }

    // initialize root component
    _solverModel.arithm(_solution.isSelectedVar, "=", 1).post()
    _solverModel.post(_solution._buildConstraintsClause)

    // configure utility
    _utility = _solution._collectUtility
    _utility match {
      case Some(config.solverModel.IntegerIntVar(utility)) =>
        _solverModel.setObjective(Model.MAXIMIZE, utility)
      case _ => // utility is constant or unset, so we ignore it
    }

    config.solverModel.init()
  }

  def solutionUtility: Int = _utility.map(_.asInt).getOrElse(0)

  def solverLimitTime(limit: Long) =
    _solverModel.getSolver.limitTime(limit)

  def solve(): Boolean = {
    _actions = null
    _solverModel.solveAndRecord()
  }

  def instance: EnsembleType = _solution

  def exists: Boolean = _solverModel.exists

  def actions: Iterable[Action] = _actions

  def commit(): Unit = {
    _actions = _solution._collectActions()
  }

  def resolve(): Boolean = {
    init()
    _utility match {
      // repeatedly solve to optimize objective
      case Some(u) => while (solve()) {}
      // solve and record first solution
      case None => solve()
    }
    if (exists) commit()
    return exists
  }
}

object Policy {
  def root[E <: Ensemble](builder: => E): Policy[E] =
    new Policy(builder _)
}
