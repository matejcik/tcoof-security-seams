package tcof

/**
  * Collection of members (components) kept in a role
  */
abstract class RoleMembers[+ComponentType <: Component](
    values: Iterable[ComponentType]
) extends Members(values)
    with WithConfig {
  private[tcof] def mapChildToParent(membersContainer: WithMembers[Component])
}
