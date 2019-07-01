package tcof

import tcof.InitStages.InitStages

trait Initializable {
  private[tcof] var _config: Config = _

  private[tcof] def _solverModel = _config.solverModel

  private[tcof] def _init(stage: InitStages, config: Config): Unit = {
    stage match {
      case InitStages.ConfigPropagation =>
        _config = config
      case _ =>
    }
  }
}
