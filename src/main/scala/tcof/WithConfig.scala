package tcof

import tcof.InitStages.InitStages

trait WithConfig extends Initializable {
  private[tcof] var _config: Config = _

  private[tcof] def _solverModel = _config.solverModel

  override private[tcof] def _init(stage: InitStages, config: Config) = {
    super._init(stage, config)

    stage match {
      case InitStages.ConfigPropagation =>
        _config = config
      case _ =>
    }
  }
}
