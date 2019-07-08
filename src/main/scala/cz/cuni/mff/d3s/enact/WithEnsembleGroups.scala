package cz.cuni.mff.d3s.enact

import InitStages.InitStages
import Utils._

import scala.collection.mutable

trait WithEnsembleGroups extends Initializable with CommonImplicits {
  this: WithSelectionStatus =>

  def rules[E <: Ensemble](ensFirst: E, ensRest: E*): EnsembleGroup[E] =
    rules(ensFirst +: ensRest)

  def rules[E <: Ensemble](ens: Iterable[E]): EnsembleGroup[E] =
    _addEnsembleGroup("rules_" + randomName, ens, true)

  /** A set of all potential ensembles */
  private[enact] val _ensembleGroups = mutable.ArrayBuffer.empty[EnsembleGroup[Ensemble]]

  def ensembles[E <: Ensemble](ensFirst: E, ensRest: E*): EnsembleGroup[E] =
    ensembles(ensFirst +: ensRest)

  def ensembles[E <: Ensemble](ens: Iterable[E]): EnsembleGroup[E] =
    _addEnsembleGroup("ensembles_" + randomName, ens, false)

  def _addEnsembleGroup[EnsembleType <: Ensemble](
      name: String,
      ens: Iterable[EnsembleType],
      enforceSituation: Boolean,
  ): EnsembleGroup[EnsembleType] = {
    val group =
      new EnsembleGroup(name, ens, enforceSituation)
    _ensembleGroups += group
    group
  }

  override private[enact] def _init(stage: InitStages, config: Config): Unit = {
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
