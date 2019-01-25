package tcof

import scala.reflect.ClassTag

case class RoleMembersUnionMember[+MemberType](value: MemberType, indicesInParents: Iterable[(WithMembers[_], Int)])

/**
  * Collection of members that stems from another parent role. A member in this collection can be selected if and only if it is selected in the parent role.
  */
class RoleMembersUnion[+ComponentType <: Component](val linkedMembers: Seq[RoleMembersUnionMember[ComponentType]]) extends RoleMembers(linkedMembers.map(_.value)) {

  private[tcof] override def mapChildToParent(membersContainer: WithMembers[Component]): Unit = {
    val members = linkedMembers.zipWithIndex

    for ((member, idx) <- members) {
      val vrs =
        for ((parent, indexInParent) <- member.indicesInParents) yield
          _solverModel.member(indexInParent, parent.allMembersVar).reify()

      _solverModel.ifOnlyIf(_solverModel.member(idx, membersContainer.allMembersVar), _solverModel.or(vrs.toArray : _*))
    }
  }

}
