package tcof

import tcof.InitStages.InitStages

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

  override def _init(
    stage: InitStages, config: Config
  ): Unit = {
    val members = linkedMembers.zipWithIndex

    for ((member, idx) <- members) {
      val vrs =
        for ((parent, pidx) <- member.indicesInParents)
          yield _solverModel.member(pidx, parent.allMembersVar).reify()

      _solverModel.ifOnlyIf(
        _solverModel.member(idx, allMembersVar),
        _solverModel.or(vrs.toArray: _*)
      )
    }
  }
}
