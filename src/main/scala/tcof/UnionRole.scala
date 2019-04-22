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
    linkedMembers: Map[ComponentType, Iterable[(Role[_], Int)]],
    cardinalityConstraint: Integer => Logical,
) extends Role(name, linkedMembers.keys, cardinalityConstraint) {

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
          .map((x: ComponentType) =>
            x -> mutable.ListBuffer.empty[(Role[_], Int)])
          .toMap

        for (role <- roles) {
          for ((member, idx) <- role.allMembers.zipWithIndex) {
            val entry = (role, idx)
            linkedMembers(member) += entry
          }
        }

        linkedMembers
      },
      cardinalityConstraint
    )

  override def _init(
      stage: InitStages,
      config: Config
  ): Unit = {
    super._init(stage, config)

    stage match {
      case InitStages.RulesCreation =>
        val members = allMembers.zipWithIndex

        for ((member, idx) <- members) {
          val vrs = for ((parent, pidx) <- linkedMembers(member))
            yield _solverModel.member(pidx, parent.allMembersVar).reify()

          _solverModel.ifOnlyIf(
            _solverModel.member(idx, allMembersVar),
            _solverModel.or(vrs.toSeq: _*))
        }
      case _ =>
    }
  }
}
