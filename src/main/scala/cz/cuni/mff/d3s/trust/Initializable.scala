package cz.cuni.mff.d3s.trust

import InitStages.InitStages

/** Base trait for initialization propagation.
  *
  * Provides the mechanism for hooking into initialization process, and access to the
  * solver instance.
  *
  * See thesis section 6.2.6 for detailed description.
  */
private[trust] trait Initializable {

  /** Config instance for the current policy. */
  private[trust] var _config: Config = _

  /** Reference to the current solver object.
    *
    * @return a shortcut to the appropriate field of [[_config]].
    */
  private[trust] def _solverModel = _config.solverModel

  /** Run the initialization process.
    *
    * Implementers of this trait are supposed to override this method and insert their
    * own initialization requirements. Collections must propagate the call to `_init`
    * to their members. Implementers must always call the parent implementation.
    *
    * The initialization runs in three phases:
    * <ul>
    * <li> [[InitStages.ConfigPropagation]]: references to the global configuration
    *      instance are registered
    * <li> [[InitStages.VarsCreation]]: solver variables are generated
    * <li> [[InitStages.RulesCreation]]: concrete rules and constraints are posted
    * </ul>
    *
    * The base implementation handles the `ConfigPropagation` stage.
    *
    * @param stage Current initialization phase
    * @param config Policy configuration object being propagated
    */
  private[trust] def _init(stage: InitStages, config: Config): Unit = {
    stage match {
      case InitStages.ConfigPropagation =>
        _config = config
      case _ =>
    }
  }
}
