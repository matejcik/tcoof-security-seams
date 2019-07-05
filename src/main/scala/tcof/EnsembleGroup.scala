package tcof

import tcof.InitStages.InitStages
import tcof.Utils._

class EnsembleGroup[+EnsembleType <: Ensemble](
    val name: String,
    values: Iterable[EnsembleType],
    private[tcof] val extraRulesFn: (
        EnsembleGroup[Ensemble],
        Logical,
        Iterable[Logical]
    ) => Unit
) extends MemberGroup(values)
    with Initializable
    with CommonImplicits {

  private[tcof] def allMembersVarName: String = "EG_" + name

  private[tcof] var parentGroup: EnsembleGroup[_ <: Ensemble] = null
  private[tcof] var indexInParentGroup: Int = _

  override private[tcof] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)
    allMembers.foreach(_._init(stage, config))

    stage match {
      case InitStages.ConfigPropagation =>
        for ((ensemble, idx) <- allMembers.zipWithIndex) {
          for (group <- ensemble._ensembleGroups.values) {
            group.parentGroup = this
            group.indexInParentGroup = idx
          }
        }

      case InitStages.RulesCreation =>
        var ensembleGroupActive: Logical = LogicalBoolean(true)

        if (parentGroup != null) {
          val ensembleGroupActiveCond =
            _solverModel.member(indexInParentGroup, parentGroup.allMembersVar)
          ensembleGroupActive = LogicalBoolVar(ensembleGroupActiveCond.reify())

          for (idx <- allMembers.indices) {
            _solverModel.ifThen(
              _solverModel.member(idx, allMembersVar),
              ensembleGroupActiveCond
            )
          }
          // TODO skoro by se chtělo říct že tohle by šlo nahradit:
          /*
          _solverModel.ifThen(
            _solverModel.arithm(allMembersVar.getCard, ">", 0),
            ensembleGroupActiveCond
          )
          */
        }

        val constraintsClauses = allMembers.map(
          ens => ens._isInSituation && ens._buildConstraintsClause
        )

        _solverModel.post(
          _solverModel.forAllSelected(constraintsClauses, allMembersVar)
        )

        if (extraRulesFn != null) {
          extraRulesFn(this, ensembleGroupActive, constraintsClauses)
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
