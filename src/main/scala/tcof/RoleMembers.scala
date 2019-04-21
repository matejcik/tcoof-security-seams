package tcof

/**
  * Collection of members (components) kept in a role
  */
abstract class RoleMembers[+ComponentType <: Component](
    name: String,
    values: Iterable[ComponentType]
) extends WithMembers(values)
    with WithConfig {

  override def allMembersVarName: String = "RM_" + name
  private[tcof] def mapChildToParent(membersContainer: WithMembers[Component])
}
