package tcof

import org.chocosolver.solver.constraints.Constraint
import tcof.InitStages.InitStages
import tcof.Utils._

class EnsembleGroup[+EnsembleType <: Ensemble]
    (val name: String, private[tcof] val allMembers: EnsembleGroupMembers[EnsembleType], private[tcof] val extraRulesFn: (EnsembleGroup[Ensemble], Logical, Iterable[Logical]) => Unit)
    extends WithMembers[EnsembleType] with WithConfig with CommonImplicits {

  private[tcof] def allMembersVarName: String = "EG_" + name

  private[tcof] var parentGroup: EnsembleGroup[_ <: Ensemble] = null
  private[tcof] var indexInParentGroup: Int = _

  override private[tcof] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)
    allMembers.values.foreach(_._init(stage, config))

    stage match {
      case InitStages.ConfigPropagation =>
        for ((ensemble, idx) <- allMembers.values.zipWithIndex) {
          for (group <- ensemble._ensembleGroups.values) {
            group.parentGroup = this
            group.indexInParentGroup = idx
          }
        }

      case InitStages.RulesCreation =>
        var ensembleGroupActive: Logical = LogicalBoolean(true)

        if (parentGroup != null) {
          val ensembleGroupActiveCond = _solverModel.member(indexInParentGroup, parentGroup.allMembersVar)
          ensembleGroupActive = LogicalBoolVar(ensembleGroupActiveCond.reify())

          for (idx <- 0 until allMembers.size) {
            _solverModel.ifThen(_solverModel.member(idx, allMembersVar), ensembleGroupActiveCond)
          }
        }

        val constraintsClauses = allMembers.map(ens => ens._isInSituation && ens._buildConstraintsClause)

        _solverModel.post(_solverModel.forAllSelected(constraintsClauses, allMembersVar))

        if (extraRulesFn != null) {
          extraRulesFn(this, ensembleGroupActive, constraintsClauses)
        }

      case _ =>
    }

  }

  override def toString: String =
    s"""Ensemble group "$name":\n${indent(selectedMembers.mkString(""), 1)}"""

  def toStringWithUtility: String =
    s"""Ensemble group "$name":\n${indent(selectedMembers.map(_.toStringWithUtility).mkString(""), 1)}"""
}
