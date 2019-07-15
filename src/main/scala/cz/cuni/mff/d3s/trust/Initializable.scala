package cz.cuni.mff.d3s.trust

import InitStages.InitStages

trait Initializable {
  private[trust] var _config: Config = _

  private[trust] def _solverModel = _config.solverModel

  private[trust] def _init(stage: InitStages, config: Config): Unit = {
    stage match {
      case InitStages.ConfigPropagation =>
        _config = config
      case _ =>
    }
  }
}
