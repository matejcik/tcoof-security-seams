package tcof

import tcof.InitStages.InitStages

import scala.collection.mutable

class UnionRole[+ComponentType <: Component](
    name: String,
    roles: Iterable[Role[ComponentType]],
    linkedMembers: Map[ComponentType, Iterable[(Role[_], Int)]],
) extends Role(name, linkedMembers.keys) {

  def this(
      name: String,
      roles: Iterable[Role[ComponentType]],
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
      }
    )

  override private[tcof] def _init(
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
            _solverModel.or(vrs.toSeq: _*)
          )
        }
      case _ =>
    }
  }
}
