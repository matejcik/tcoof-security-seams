package tcof

import org.chocosolver.solver.constraints.Constraint
import tcof.InitStages.InitStages
import tcof.Utils._

import scala.collection.mutable

trait WithEnsembleGroups extends Initializable with CommonImplicits {
  this: WithConfig =>

  def rules[EnsembleType <: Ensemble](ensFirst: EnsembleType, ensRest: EnsembleType*): EnsembleGroup[EnsembleType] = rules(ensRest.+:(ensFirst))

  def rules[EnsembleType <: Ensemble](ens: Iterable[EnsembleType]): EnsembleGroup[EnsembleType] =
    _addEnsembleGroup("rules_" + randomName, ens,
      (ensGroup, ensembleGroupActive, _) => _solverModel.postEnforceSelected(ensGroup.allMembers.map(ens => ens._isInSituation && ensembleGroupActive), ensGroup.allMembersVar)
    )

  /** A set of all potential ensembles */
  private[tcof] val _ensembleGroups = mutable.Map.empty[String, EnsembleGroup[Ensemble]]

  /*
  def ensembles[EnsembleType <: Ensemble](ensFirst: EnsembleType, ensRest: EnsembleType*): EnsembleGroup[EnsembleType] = ensembles(randomName, ensRest.+:(ensFirst))

  def ensembles[EnsembleType <: Ensemble](ens: Iterable[EnsembleType]): EnsembleGroup[EnsembleType] = ensembles(randomName, ens)

  def ensembles[EnsembleType <: Ensemble](name: String, ensFirst: EnsembleType, ensRest: EnsembleType*): EnsembleGroup[EnsembleType] = ensembles(name, ensRest.+:(ensFirst))

  def ensembles[EnsembleType <: Ensemble](name: String, ens: Iterable[EnsembleType]): EnsembleGroup[EnsembleType] = _addEnsembleGroup(name, ens, false)
  */

  def _addEnsembleGroup[EnsembleType <: Ensemble](name: String, ens: Iterable[EnsembleType], extraRulesFn: (EnsembleGroup[Ensemble], Logical, Iterable[Logical]) => Unit): EnsembleGroup[EnsembleType] = {
    val group = new EnsembleGroup(name, new EnsembleGroupMembers(ens), extraRulesFn)
    _ensembleGroups += name -> group
    group
  }

  override private[tcof] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)
    _ensembleGroups.values.foreach(_._init(stage, config))
  }
}
