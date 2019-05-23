package tcof

import tcof.InitStages.InitStages
import tcof.Utils._

import scala.collection.mutable

trait WithEnsembleGroups extends Initializable with CommonImplicits {
  this: WithConfig =>

  def rules[E <: Ensemble](ensRest: E*): EnsembleGroup[E] = rules(ensRest)

  def rules[E <: Ensemble](ens: Iterable[E]): EnsembleGroup[E] =
    _addEnsembleGroup(
      "rules_" + randomName,
      ens,
      (ensGroup, ensembleGroupActive, _) =>
        _solverModel.postEnforceSelected(
          ensGroup.allMembers
            .map(ens => ens._isInSituation && ensembleGroupActive),
          ensGroup.allMembersVar
        )
    )

  /** A set of all potential ensembles */
  private[tcof] val _ensembleGroups =
    mutable.Map.empty[String, EnsembleGroup[Ensemble]]

  /*
  def ensembles[EnsembleType <: Ensemble](ensFirst: EnsembleType, ensRest: EnsembleType*): EnsembleGroup[EnsembleType] = ensembles(randomName, ensRest.+:(ensFirst))

  def ensembles[EnsembleType <: Ensemble](ens: Iterable[EnsembleType]): EnsembleGroup[EnsembleType] = ensembles(randomName, ens)

  def ensembles[EnsembleType <: Ensemble](name: String, ensFirst: EnsembleType, ensRest: EnsembleType*): EnsembleGroup[EnsembleType] = ensembles(name, ensRest.+:(ensFirst))

  def ensembles[EnsembleType <: Ensemble](name: String, ens: Iterable[EnsembleType]): EnsembleGroup[EnsembleType] = _addEnsembleGroup(name, ens, false)
   */

  def _addEnsembleGroup[EnsembleType <: Ensemble](
      name: String,
      ens: Iterable[EnsembleType],
      extraRulesFn: (
          EnsembleGroup[Ensemble],
          Logical,
          Iterable[Logical]
      ) => Unit
  ): EnsembleGroup[EnsembleType] = {
    val group =
      new EnsembleGroup(name, ens, extraRulesFn)
    _ensembleGroups += name -> group
    group
  }

  override private[tcof] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)
    _ensembleGroups.values.foreach(_._init(stage, config))
  }
}
