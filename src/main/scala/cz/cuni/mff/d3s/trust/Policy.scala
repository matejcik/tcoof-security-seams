package cz.cuni.mff.d3s.trust

import org.chocosolver.solver.Model

/** Security policy handler.
  *
  * Provides an interface to the solver, methods controlling the solving process,
  * access to the solution, and access control queries.
  *
  * @param builder Constructor expression of the ensemble hierarchy.
  * @tparam EnsembleType Concrete type of the root ensemble.
  */
class Policy[EnsembleType <: Ensemble](builder: () => EnsembleType) {

  /** Reference to an instance of the ensemble hierarchy. */
  private var _solution: EnsembleType = _

  /** Reference to an utility value of the solution. */
  private var _utility: Option[Integer] = None

  /** List of collected security actions. */
  private var _actions: Iterable[Action] = null

  /** Set of collected security actions, for fast action lookup. */
  private var _actionSet: Set[Action] = Set.empty

  /** Solver instance. */
  private var _solverModel: SolverModel = _

  /** Reset list of security actions. */
  private def _resetActions(): Unit = {
    _actions = null
    _actionSet = Set.empty
  }

  /** Initialize the solution process.
    *
    * Generates a new instance of the ensemble hierarchy from the `builder` function,
    * clears the action lists, sets a solution time limit if specifed, initializes
    * the solver and the ensembles.
    *
    * A new solver instance must be created every time, otherwise it retains previously
    * posted constraints.
    *
    * A new instance of the ensemble hierarchy must be created every time, because the
    * ensemble objects often close over external values, which need to be refreshed.
    *
    * If any utility expression is found, the solver is configured to maximize it.
    * This must only be done if an utility expression exists. Otherwise, we could
    * configure the solver to maximize a constant, but then the first solution would
    * be optimal and we could not iterate over other possible solutions.
    *
    * @param limit Time limit in milliseconds. If a solving attempt takes more than
    *              `limit` ms, it is stopped.
    */
  def init(limit: Long = -1): Unit = {
    _resetActions()
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

    _solverModel.init()
    if (limit > -1) _solverModel.getSolver.limitTime(limit)
  }

  /** Utility value of latest solution.
    *
    * If no utility expressions were configured, the value is zero.
    */
  def solutionUtility: Int = _utility.map(_.asInt).getOrElse(0)

  /** Find next solution.
    *
    * Instructs the solver to find next solution, and makes it available for examining.
    *
    * @return true if a new solution was found, false otherwise.
    */
  def solve(): Boolean = {
    _resetActions()
    _solverModel.solveAndRecord()
  }

  /** Reference to the root ensemble instance. */
  def instance: EnsembleType = _solution

  /** True if a solution exists. */
  def exists: Boolean = _solverModel.exists

  /** List of collected security actions.
    *
    * [[commit]] must be called after [[solve]] in order for `actions` to become
    * available.
    */
  def actions: Iterable[Action] = _actions

  /** Query the security rules.
    *
    * Implements a default-deny policy. `actor` is allowed to perform `action`
    * on `subject` if there is an `allow` action with the appropriate values, and
    * there is no `deny` action with the same values.
    *
    * In other words: Actions are denied by default. `allow` calls whitelist some actions,
    * and `deny` calls can override the whitelist.
    *
    * @param actor Actor component
    * @param action Action name
    * @param subject Subject component
    * @return true if the action is allowed, false otherwise.
    */
  def allows(actor: Component, action: String, subject: Component): Boolean = {
    if (_actionSet contains DenyAction(actor, action, subject)) false
    else _actionSet contains AllowAction(actor, action, subject)
  }

  /** Collect security actions and send notifications.
    *
    * If `commit()` is not called, no notifications are sent.
    *
    * It must also be called in order to access the [[actions]] attribute.
    */
  def commit(): Unit = {
    _actions = _solution._collectActions()
    _actionSet = _actions.toSet
  }

  /** Resolve the policy.
    *
    * An all-in-one method to initialize the solver and find the optimal solution in
    * terms of utility. If no utility is specified, find the first solution.
    *
    * @param limit Time limit in milliseconds
    * @return true if a solution was found within the time limit, false otherwise.
    */
  def resolve(limit: Long = -1): Boolean = {
    init(limit)

    _utility match {
      // repeatedly solve to optimize objective
      case Some(_) => while (solve()) {}
      // solve and record first solution
      case None => solve()
    }
    if (exists) commit()
    return exists
  }
}

/** Policy companion object. */
object Policy {

  /** Create a policy root.
    *
    * DSL function to register the root ensemble. Typical usage:
    * {{{
    *   class MyEnsemble extends Ensemble {
    *     /* ... */
    *   }
    *
    *   val policy = Policy.root(new Ensemble)
    * }}}
    *
    * @param builder Expression that creates an instance of the root ensemble
    * @tparam E Concrete type of the root ensemble
    * @return a policy object wrapping the ensemble hierarchy.
    */
  def root[E <: Ensemble](builder: => E): Policy[E] = new Policy(builder _)
}
