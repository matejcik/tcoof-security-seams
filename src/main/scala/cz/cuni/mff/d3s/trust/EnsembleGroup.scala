package cz.cuni.mff.d3s.trust

import InitStages.InitStages
import Utils._

class EnsembleGroup[+EnsembleType <: Ensemble](
    name: String,
    values: Iterable[EnsembleType],
    val enforceSituation: Boolean,
) extends MemberGroup(name, values)
    with Initializable
    with CommonImplicits {

  override private[trust] val allMembersVarName: String = "EG_" + name

  override private[trust] def _init(stage: InitStages, config: Config): Unit = {
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

  override def toString: String = s"<EnsembleGroup:$allMembersVarName:$name>"
}
