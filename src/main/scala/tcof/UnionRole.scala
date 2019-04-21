package tcof

import scala.collection.mutable

case class UnionRoleMember[+MemberType](
    value: MemberType,
    indicesInParents: Iterable[(Role[_], Int)],
)

class UnionRole[+ComponentType <: Component](
    name: String,
    roles: Iterable[Role[ComponentType]],
    linkedMembers: Iterable[UnionRoleMember[ComponentType]],
    cardinalityConstraint: Integer => Logical,
) extends Role(name, linkedMembers.map(_.value), cardinalityConstraint) {

  def this(
      name: String,
      roles: Iterable[Role[ComponentType]],
      cardinalityConstraint: Integer => Logical = null,
  ) =
    this(
      name,
      roles, {
        val linkedMembers = roles
          .flatMap(_.allMembers)
          .toSet
          .map((x: ComponentType) => x -> mutable.ListBuffer.empty[(Role[_], Int)])
          .toMap

        for (role <- roles) {
          for ((member, idx) <- role.allMembers.zipWithIndex) {
            val entry = (role, idx)
            linkedMembers(member) += entry
          }
        }

        for (member <- linkedMembers.keys)
          yield
            UnionRoleMember(
              member,
              linkedMembers(member).toIterable
            )
      },
      cardinalityConstraint
    )
}
