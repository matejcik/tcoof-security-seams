package cz.cuni.mff.d3s.enact

import InitStages.InitStages
import Utils._

import scala.collection.mutable

class UnionGroup[+ComponentType <: Component] private(
    name: String,
    groups: Iterable[MemberGroup[ComponentType]],
    linkedMembers: Map[ComponentType, Iterable[(MemberGroup[_], Int)]],
) extends MemberGroup(name, linkedMembers.keys) {

  def this(roles: Iterable[MemberGroup[ComponentType]]) =
    this(
      "unionOf_" + roles.map(_.name).mkString("_"),
      roles, {
        val linkedMembers = roles
          .flatMap(_.allMembers)
          .toSet
          .map((x: ComponentType) => x -> mutable.ListBuffer.empty[(MemberGroup[_], Int)])
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

  override private[enact] def _init(
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
