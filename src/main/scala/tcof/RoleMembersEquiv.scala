package tcof

import scala.reflect.ClassTag

case class RoleMembersEquivMember[+MemberType](
    value: MemberType,
    parent: WithMembers[_],
    indexInParent: Int
)

/**
  * Collection of members that stems from another parent role. A member in this collection can be selected if and only if it is selected in the parent role.
  */
class RoleMembersEquiv[+ComponentType <: Component](
    val linkedMembers: Iterable[RoleMembersEquivMember[ComponentType]]
) extends RoleMembers(linkedMembers.map(_.value)) {

  /** Creates members from existing parent without any filtering. */
  def this(parent: WithMembers[ComponentType]) =
    this(parent.allMembers.values.zipWithIndex.map {
      case (member, idx) => RoleMembersEquivMember(member, parent, idx)
    })

  private[tcof] override def mapChildToParent(
      membersContainer: WithMembers[Component]
  ): Unit = {
    val members = linkedMembers.zipWithIndex
    for ((member, idx) <- members) {
      _solverModel.ifOnlyIf(
        _solverModel.member(idx, membersContainer.allMembersVar),
        _solverModel.member(member.indexInParent, member.parent.allMembersVar)
      )
    }
  }

  def selectEquiv[RoleType <: Component: ClassTag]: RoleMembersEquiv[RoleType] =
    new RoleMembersEquiv(linkedMembers.collect {
      case member @ RoleMembersEquivMember(
            value: RoleType,
            parent,
            indexInParent
          ) =>
        RoleMembersEquivMember(
          value.asInstanceOf[RoleType],
          parent,
          indexInParent
        )
    })

  def ++[B >: ComponentType <: Component](
      other: RoleMembersEquiv[B]
  ): RoleMembersEquiv[B] = {
    val members = linkedMembers ++ other.linkedMembers
    new RoleMembersEquiv(members)
  }

}
