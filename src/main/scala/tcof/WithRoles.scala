package tcof

import tcof.InitStages.InitStages
import tcof.Utils._

import scala.collection.mutable

trait WithRoles extends Initializable with CommonImplicits {
  this: WithConfig =>

  private[tcof] val _roles: mutable.Map[String, Role[Component]] =
    mutable.Map.empty[String, Role[Component]]

  def oneOf[ComponentType <: Component](
    items: RoleMembers[ComponentType]
  ): Role[ComponentType] =
    _addRole("oneOf_" + randomName, items, cardinality => cardinality === 1)

  def unionOf[ComponentType <: Component](
    roles: Iterable[Role[ComponentType]]
  ): Role[ComponentType] = {
    val allMembersWithParentIndices = roles
      .flatMap(_.allMembers.values)
      .toSet
      .map((x: ComponentType) => (x, mutable.ListBuffer.empty[(Role[_], Int)]))
      .toMap

    for (role <- roles) {
      for ((member, idx) <- role.allMembers.values.zipWithIndex) {
        val entry = (role, idx)
        allMembersWithParentIndices(member) += entry
      }
    }

    val items =
      for (member <- allMembersWithParentIndices.keys)
        yield
          new RoleMembersUnionMember(
            member,
            allMembersWithParentIndices(member)
          )

    _addRole("unionOf_" + randomName, new RoleMembersUnion(items.toList), null)
  }

  def _addRole[ComponentType <: Component](
    name: String,
    items: RoleMembers[ComponentType],
    cardinalityConstraints: Integer => Logical
  ): Role[ComponentType] = {
    val role =
      new Role[ComponentType](name, this, items, cardinalityConstraints)
    _roles += name -> role
    role
  }

  override private[tcof] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)
    _roles.values.foreach(_._init(stage, config))
  }
}
