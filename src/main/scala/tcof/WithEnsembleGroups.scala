package tcof

import org.chocosolver.solver.variables.BoolVar
import tcof.InitStages.InitStages
import tcof.Utils._

import scala.collection.mutable

trait WithEnsembleGroups extends Initializable with CommonImplicits {

  def rules[E <: Ensemble](ensFirst: E, ensRest: E*): EnsembleGroup[E] = rules(ensFirst +: ensRest)

  def rules[E <: Ensemble](ens: Iterable[E]): EnsembleGroup[E] =
    _addEnsembleGroup("rules_" + randomName, ens, true)

  private[tcof] var isSelectedVar: BoolVar = _

  /** A set of all potential ensembles */
  private[tcof] val _ensembleGroups = mutable.Map.empty[String, EnsembleGroup[Ensemble]]


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
    _ensembleGroups += name -> group
    group
  }

  override private[tcof] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)
    _ensembleGroups.values.foreach(_._init(stage, config))

    stage match {
      case InitStages.VarsCreation =>
        isSelectedVar = _solverModel.boolVar()
      case InitStages.RulesCreation =>
        for (group <- _ensembleGroups.values)
          _solverModel.arithm(group.isActiveVar, "=", isSelectedVar).post()
      case _ =>
    }
  }

  def selected: Boolean = _solverModel.solution.getIntVal(isSelectedVar) == 1
}
