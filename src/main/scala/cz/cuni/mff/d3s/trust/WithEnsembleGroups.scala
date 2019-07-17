package cz.cuni.mff.d3s.trust

import InitStages.InitStages
import Utils._

import scala.collection.mutable

/** Ensemble part for handling sub-ensembles.
  *
  * Provides DSL functions that register sub-ensembles.
  */
trait WithEnsembleGroups extends Initializable with CommonImplicits {
  this: WithSelectionStatus =>

  /** A set of all sub-ensemble groups */
  private[trust] val _ensembleGroups = mutable.ArrayBuffer.empty[EnsembleGroup[Ensemble]]

  /** Register sub-ensembles with mandatory activation.
    *
    * Ensures that at least one argument is passed. Delegates to `rules(Iterable[E])`
    */
  def rules[E <: Ensemble](ensFirst: E, ensRest: E*): EnsembleGroup[E] =
    rules(ensFirst +: ensRest)

  /** Register sub-ensembles with mandatory activation.
    *
    * All ensembles in `ens` must be selected if their situation predicate is true.
    *
    * @param ens Sub-ensembles
    * @tparam E Concrete sub-ensemble type
    * @return an `EnsembleGroup` representing the ensembles.
    */
  def rules[E <: Ensemble](ens: Iterable[E]): EnsembleGroup[E] =
    _addEnsembleGroup("rules_" + randomName, ens, true)

  /** Register sub-ensembles with optional activation.
    *
    * Ensures that at least one argument is passed. Delegates to `ensembles(Iterable[E])`
    */
  def ensembles[E <: Ensemble](ensFirst: E, ensRest: E*): EnsembleGroup[E] =
    ensembles(ensFirst +: ensRest)

  /** Register sub-ensembles with optional activation.
    *
    * Ensembles in the resulting group may or may not be activated, based on the solver
    * decisions.
    *
    * @param ens Sub-ensembles
    * @tparam E Concrete sub-ensemble type
    * @return an `EnsembleGroup` representing the ensembles.
    */
  def ensembles[E <: Ensemble](ens: Iterable[E]): EnsembleGroup[E] =
    _addEnsembleGroup("ensembles_" + randomName, ens, false)

  /** Register an ensemble group.
    *
    * Internal common handling for `rules` and `ensembles`.
    *
    * @param name Ensemble group name
    * @param ens Sub-ensembles
    * @param enforceSituation True if mandatory activation is requested
    * @tparam E Concrete sub-ensemble type
    * @return an `EnsembleGroup` representing the ensembles.
    */
  private[trust] def _addEnsembleGroup[E <: Ensemble](
      name: String,
      ens: Iterable[E],
      enforceSituation: Boolean
  ): EnsembleGroup[E] = {
    val group =
      new EnsembleGroup(name, ens, enforceSituation)
    _ensembleGroups += group
    group
  }

  /** Initialization hook.
    *
    * Propagates [[_init()]] call to members and installs a dependency of the
    * sub-ensemble group activation on the selection status of this ensemble.
    */
  override private[trust] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)
    _ensembleGroups.foreach(_._init(stage, config))

    stage match {
      case InitStages.RulesCreation =>
        for (group <- _ensembleGroups)
          _solverModel.arithm(group.isActiveVar, "=", isSelectedVar).post()
      case _ =>
    }
  }
}
