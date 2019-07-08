package cz.cuni.mff.d3s.enact

import InitStages.InitStages

trait WithUtility extends Initializable {

  private var _utilityFun: Option[() => Integer] = None

  def utility(util: => Integer): Unit = {
    _utilityFun = Some(util _)
  }

  private var _utility: Option[Integer] = None

  private[enact] def _getUtility: Option[Integer] = _utility

  def utility: Integer = _getUtility.getOrElse(_solverModel.IntegerInt(0))

  def solutionUtility: Int = _utility match {
    case Some(value) => value.asInt
    case None        => 0
  }

  override private[enact] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)

    stage match {
      case InitStages.RulesCreation =>
        _utility = _utilityFun.map(_.apply())
      case _ =>
    }
  }
}
