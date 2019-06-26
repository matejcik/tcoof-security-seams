package tcof

import org.chocosolver.solver.Model

class Scenario[EnsembleType <: Ensemble](builder: () => EnsembleType) {
  private var _solution: EnsembleType = _
  private var _utility: Option[Integer] = None
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
    _utility = _solution._collectUtility
    _utility match {
      case Some(_solverModel.IntegerIntVar(utility)) =>
        _solverModel.setObjective(Model.MAXIMIZE, utility)
      case _ => // utility is constant or unset, so we ignore it
    }

    config.solverModel.init()
  }

  def solutionUtility: Int = _utility.map(_.asInt).getOrElse(0)

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
    _utility match {
      // repeatedly solve to optimize objective
      case Some(u) => while (solve()) {}
      // solve and record first solution
      case None    => solve()
    }
    if (exists) commit()
    return exists
  }
}

object Scenario {
  def root[E <: Ensemble](builder: => E): Scenario[E] =
    new Scenario(builder _)
}
