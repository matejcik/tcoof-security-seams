package cz.cuni.mff.d3s.enact

import InitStages.InitStages

trait Initializable {
  private[enact] var _config: Config = _

  private[enact] def _solverModel = _config.solverModel

  private[enact] def _init(stage: InitStages, config: Config): Unit = {
    stage match {
      case InitStages.ConfigPropagation =>
        _config = config
      case _ =>
    }
  }
}
