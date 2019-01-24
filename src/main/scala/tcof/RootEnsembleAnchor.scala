package tcof


class RootEnsembleAnchor[EnsembleType <: RootEnsemble] private[tcof](val builder: () => EnsembleType) {
  private var _solution: EnsembleType = _

  def instance: EnsembleType = _solution

  def init(): Unit = {
    _solution = builder()

    // This is not needed per se because ensembles are discarded in each step anyway. However, component are not. We keep it here for uniformity with components.
    val solverModel = new SolverModel()
    val config = new Config(solverModel)
    for (stage <- InitStages.values) {
      _solution._init(stage, config)
    }

    solverModel.init()
  }

  def solverLimitTime(limit: String) = _solution._solverModel.getSolver.limitTime(limit)

  def solve(): Boolean = _solution._solverModel.solveAndRecord()

  def exists(): Boolean = _solution._solverModel.exists

  var _actions: Iterable[Action] = List()

  def commit(): Unit = {
    _actions = instance._collectActions()
  }

  def actions: Iterable[Action] = _actions
}

