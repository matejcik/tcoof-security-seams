package cz.cuni.mff.d3s.trust

import InitStages.InitStages

trait WithUtility extends Initializable {
  this: WithEnsembleGroups =>

  private var _utilityFun: Option[() => Integer] = None

  def utility(util: => Integer): Unit = {
    _utilityFun = Some(util _)
  }

  private var _utility: Option[Integer] = None

  private[trust] def _getUtility: Option[Integer] = _utility

  def utility: Integer = _getUtility.getOrElse(_solverModel.IntegerInt(0))

  def solutionUtility: Int = _utility match {
    case Some(value) => value.asInt
    case None        => 0
  }

  def _hasUtility: Boolean = {
    val childrenHaveUtility = _ensembleGroups.flatMap(g => g.allMembers).exists(_._hasUtility)
    _utilityFun.nonEmpty || childrenHaveUtility
  }

  def _collectUtility: Option[Integer] = {
    if (!_hasUtility) {
      None
    } else {
      val subUtilities = _ensembleGroups.map(
        g => g.sum(_._collectUtility.getOrElse(_solverModel.IntegerInt(0)))
      )
      Some(
        utility + subUtilities
          .reduceOption(_ + _)
          .getOrElse(_solverModel.IntegerInt(0))
      )
    }
  }

  override private[trust] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)

    stage match {
      case InitStages.RulesCreation =>
        _utility = _utilityFun.map(_.apply())
      case _ =>
    }
  }
}
