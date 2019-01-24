package tcof

import tcof.InitStages.InitStages

trait Initializable {
  private[tcof] def _init(stage: InitStages, config: Config): Unit = {
  }
}



