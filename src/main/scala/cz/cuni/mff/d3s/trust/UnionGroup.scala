package cz.cuni.mff.d3s.trust

import InitStages.InitStages
import scala.collection.mutable

/** Union of member groups.
  *
  * An object is selected in this group iff it is selected in at least one of the
  * constituent groups. This is used to implement the `unionOf` role function.
  *
  * The default constructor accepts a `linkedMembers` data structure which maps
  * each possible member instance to a list of constituent groups which contain
  * this member as a candidate, and indices in those groups. This data structure
  * is created in the public constructor.
  *
  * @param name Name of the group
  * @param groups Constituent groups
  * @param linkedMembers Data structure that links subgroup members to this group
  * @tparam MemberType Concrete type of members
  */
class UnionGroup[+MemberType] private (
    name: String,
    groups: Iterable[MemberGroup[MemberType]],
    linkedMembers: Map[MemberType, Iterable[(MemberGroup[_], Int)]],
) extends MemberGroup(name, linkedMembers.keys) {

  /** Public constructor
    *
    * Accepts a list of constituent groups and creates links between their members and
    * members of the union.
    *
    * @param roles Constituent groups
    */
  def this(roles: Iterable[MemberGroup[MemberType]]) =
    this(
      "unionOf_" + roles.map(_.name).mkString("_"),
      roles, {
        val linkedMembers = roles
          .flatMap(_.allMembers)
          .toSet
          .map((x: MemberType) => x -> mutable.ListBuffer.empty[(MemberGroup[_], Int)])
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

  /** Initialization hook.
    *
    * Installs rules that an object can be a member of the union group if and only if
    * it is a member of at least one of the constituent groups.
    */
  override private[trust] def _init(stage: InitStages, config: Config): Unit = {
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
