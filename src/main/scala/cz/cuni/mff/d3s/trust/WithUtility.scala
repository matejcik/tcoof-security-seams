package cz.cuni.mff.d3s.trust

import InitStages.InitStages

/** Ensemble part for handling utility expressions
  *
  * Provides a DSL function for setting the utility expression, and functionality that
  * collects utilities from sub-ensembles.
  */
trait WithUtility extends Initializable {
  this: WithEnsembleGroups =>

  /** Configured utility function. */
  private var _utilityFun: Option[() => Integer] = None

  /** Cached result of the utility function.
    *
    * The result must be cached, because otherwise each run of the utility function
    * generates a new `IntVar`. Because the result is being registered for optimization
    * by the solver, producing new `IntVar`s can mean that the solver-registered
    * variable becomes stale.
    */
  private var _utility: Option[Integer] = None

  /** Set the utility expression.
    *
    * `util` is an `Integer` expression that is only evaluated when the ensemble hierarchy
    * is initialized. This way, it is possible to declare utility at ensemble specification
    * time.
    *
    * @param util Utility expression
    */
  def utility(util: => Integer): Unit = {
    _utilityFun = Some(util _)
  }

  /*  TODO this doesn't seem necessary?
  private[trust] def _getUtility: Option[Integer] = _utility

  def utility: Integer = _getUtility.getOrElse(_solverModel.IntegerInt(0))
   */

  /** Utility variable of this ensemble.
    *
    * Does not include utilities of sub-ensembles.
    */
  def utility: Integer = _utility.getOrElse(_solverModel.IntegerInt(0))

  /** Utility value of the current ensemble in the current solution.
    *
    * Can only be used when a solution exists.
    *
    * @return utility of this ensemble, or zero if no utility expression was specified.
    */
  def solutionUtility: Int = _utility match {
    case Some(value) => value.asInt
    case None        => 0
  }

  /** True if this ensemble or any of its sub-ensembles have a utility expression. */
  private[trust] def _hasUtility: Boolean = {
    val childrenHaveUtility = _ensembleGroups.flatMap(g => g.allMembers).exists(_._hasUtility)
    _utilityFun.nonEmpty || childrenHaveUtility
  }

  /** Find the total utility of this ensemble and its sub-ensembles.
    *
    * The result is an `Option[Integer]`, indicating whether an utility expression was
    * set anywhere in this sub-tree of the ensemble hierarchy. If not, the information
    * needs to be propagated to the root, so that the solver object is properly
    * configured with regard to optimization.
    *
    * @return an Integer value representing total utility of this ensemble and all its
    *         sub-ensembles, if at least one utility expression is configured. `None` if
    *         no utility expression is set in this ensemble nor in any of its sub-ensembles.
    */
  private[trust] def _collectUtility: Option[Integer] = {
    if (!_hasUtility) None
    else {
      val subUtilities = _ensembleGroups.map(
        g => g.sum(_._collectUtility.getOrElse(_solverModel.IntegerInt(0)))
      )
      val subUtilitySum = subUtilities.reduceOption(_ + _).getOrElse(_solverModel.IntegerInt(0))
      Some(utility + subUtilitySum)
    }
  }

  /** Initialization hook.
    *
    * If a utility expression is specified, its value is cached as part of the rules
    * creation step.
    */
  override private[trust] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)

    stage match {
      case InitStages.RulesCreation =>
        _utility = _utilityFun.map(_.apply())
      case _ =>
    }
  }
}
