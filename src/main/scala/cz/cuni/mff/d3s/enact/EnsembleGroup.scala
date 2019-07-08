package cz.cuni.mff.d3s.enact

import InitStages.InitStages
import Utils._

class EnsembleGroup[+EnsembleType <: Ensemble](
    val name: String,
    values: Iterable[EnsembleType],
    val enforceSituation: Boolean,
) extends MemberGroup(values)
    with Initializable
    with CommonImplicits {

  private[enact] def allMembersVarName: String = "EG_" + name

  override private[enact] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)
    allMembers.foreach(_._init(stage, config))

    stage match {
      case InitStages.RulesCreation =>
        for ((member, idx) <- allMembers.zipWithIndex)
          _solverModel.member(idx, allMembersVar).reifyWith(member.isSelectedVar)
        val constraintsClauses = allMembers.map(
          ens => ens._isInSituation && ens._buildConstraintsClause
        )
        _solverModel.post(
          _solverModel.forAllSelected(constraintsClauses, allMembersVar)
        )

        if (enforceSituation) {
          _solverModel.postEnforceSelected(
            allMembers.map(_._isInSituation && LogicalBoolVar(isActiveVar)),
            allMembersVar,
          )
        }

      case _ =>
    }

  }

  override def toString: String =
    if (_config != null && _solverModel.exists) {
      s"""Ensemble group "$name":\n${indent(selectedMembers.mkString(""), 1)}"""
    } else {
      s"""Ensemble group "$name" (unsolved):\n${indent(
        allMembers.mkString(""),
        1
      )}"""
    }

  def toStringWithUtility: String =
    s"""Ensemble group "$name":\n${indent(
      selectedMembers.map(_.toStringWithUtility).mkString(""),
      1
    )}"""
}
