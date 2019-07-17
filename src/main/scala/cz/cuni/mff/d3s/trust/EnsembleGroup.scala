package cz.cuni.mff.d3s.trust

import InitStages.InitStages

/** Group of `Ensemble`s.
  *
  * Represents a result of `rules` or `ensembles` call.
  * A collection of [[Ensemble]]s must be a [[MemberGroup]], to enable dynamic selection
  * of member components. This class adds special handling for sub-ensembles, ensuring
  * that membership in the group is reified with the ensemble's selection status.
  *
  * If `enforceSituation` is set to true, the `EnsembleGroup` also makes the membership
  * status dependent on the situation predicate of member ensembles. This facilitates
  * mandatory activation from `rules`.
  *
  * @see [[WithSelectionStatus]]
  * @param name Name of the group
  * @param values Collection of constituent ensembles
  * @param enforceSituation `true` if mandatory activation is required
  * @tparam EnsembleType Concrete type of the ensemble members
  */
class EnsembleGroup[+EnsembleType <: Ensemble](
    name: String,
    values: Iterable[EnsembleType],
    val enforceSituation: Boolean,
) extends MemberGroup(name, values)
    with Initializable
    with CommonImplicits {

  /** Name for the underlying `SetVar`. */
  override private[trust] val allMembersVarName: String = "EG_" + name

  /** Initialization hook.
    *
    * Propagates [[_init()]] call to members, installs rules that link group membership
    * to ensemble activation status, and sets up rules based on the child ensembles'
    * constraints. Optionally adds a rule for mandatory activation.
    */
  override private[trust] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)
    allMembers.foreach(_._init(stage, config))

    stage match {
      case InitStages.RulesCreation =>
        // link group membership to child activation status
        for ((member, idx) <- allMembers.zipWithIndex)
          _solverModel.member(idx, allMembersVar).reifyWith(member.isSelectedVar)

        // collect constraints from child ensembles and create solver rules from them
        val constraintsClauses = allMembers.map(
          ens => ens._isInSituation && ens._buildConstraintsClause
        )
        _solverModel.post(
          _solverModel.forAllSelected(constraintsClauses, allMembersVar)
        )

        if (enforceSituation) {
          // enforce activation when situation predicate is true
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
