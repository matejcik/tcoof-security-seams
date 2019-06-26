package tcof

import tcof.InitStages.InitStages

trait WithUtility extends Initializable {
  this: WithConfig =>

  private var _utilityFun: Option[() => Integer] = None

  def utility(util: => Integer): Unit = {
    _utilityFun = Some(util _)
  }

  private var _utility: Option[Integer] = None

  private[tcof] def _getUtility: Option[Integer] = _utility

  def utility: Integer = _getUtility.getOrElse(_solverModel.IntegerInt(0))

  def solutionUtility: Int = _utility match {
    case Some(value) => value.asInt
    case None        => 0
  }

  override private[tcof] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)

    stage match {
      case InitStages.RulesCreation =>
        _utility = _utilityFun.map(_.apply())
      case _ =>
    }
  }
}
