package tcof

import tcof.InitStages.InitStages

/** Represents a role in an ensemble. Implements methods to build membership over components contained in a role. */
class Role[+ComponentType <: Component](
    val name: String,
    values: Iterable[ComponentType],
    cardinalityConstraints: Integer => Logical
) extends MemberGroup(values)
    with WithConfig {

  override def allMembersVarName: String = "R_" + name

  override def toString: String =
    s"""Role "$name""""
//    s"""Role "$name": ${selectedMembers.map(_.toString).mkString(" ")}\n"""

  override private[tcof] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)

    stage match {
      case InitStages.RulesCreation =>
        if (cardinalityConstraints != null) {
          _solverModel.post(cardinalityConstraints(cardinality))
        }
      case _ =>
    }
  }
}
