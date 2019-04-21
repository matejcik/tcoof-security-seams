package tcof

import tcof.InitStages.InitStages
import tcof.Utils._

/** Represents a role in an ensemble. Implements methods to build membership over components contained in a role. */
class Role[+ComponentType <: Component](
    val name: String,
    private[tcof] val parent: WithRoles,
    private[tcof] val allMembers: RoleMembers[ComponentType],
    cardinalityConstraints: Integer => Logical
) extends Initializable
    with WithConfig {

  def cloneEquiv = new RoleMembersEquiv(name, allMembers)

  def ++[OtherType >: ComponentType <: Component](
      other: Role[OtherType]
  ): Role[OtherType] = {
    require(parent == other.parent)
    parent._addRole(randomName, cloneEquiv ++ other.cloneEquiv, null)
  }

  override def toString: String =
    s"""Role "$name""""
//    s"""Role "$name": ${selectedMembers.map(_.toString).mkString(" ")}\n"""

  override private[tcof] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)
    allMembers._init(stage, config)

    stage match {
      case InitStages.RulesCreation =>
        allMembers.mapChildToParent(this)

        if (cardinalityConstraints != null) {
          _solverModel.post(cardinalityConstraints(allMembers.cardinality))
        }
      case _ =>
    }
  }
}

object Role {
  implicit def roleToMembers[ComponentType <: Component](
      role: Role[ComponentType],
  ): RoleMembers[ComponentType] = role.allMembers
}
