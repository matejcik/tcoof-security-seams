package tcof

import tcof.InitStages.InitStages
import tcof.Utils._

import scala.collection.mutable

trait WithRoles extends Initializable with CommonImplicits {
  this: WithConfig =>

  private[tcof] val _roles = mutable.Map.empty[String, Role[Component]]

  implicit def roleToComponents[C <: Component](role: Role[C]): Iterable[C] =
    role.selectedMembers

  def oneOf[C <: Component](items: Iterable[C]): Role[C] =
    _addRole("oneOf_" + randomName, items, card => card === 1)

  def unionOf[C <: Component](roles: Role[C]*): Role[C] = unionOf(roles)

  def unionOf[C <: Component](roles: Iterable[Role[C]]): Role[C] =
    _addRole(new UnionRole("unionOf_" + randomName, roles))

  def subsetOf[C <: Component](
      items: Iterable[C],
      cardinality: Integer => Logical = null,
  ): Role[C] =
    _addRole("subsetOf_" + randomName, items, cardinality)

  def _addRole[C <: Component](
      name: String,
      items: Iterable[C],
      cardinality: Integer => Logical,
  ): Role[C] = _addRole(new Role(name, items, cardinality))

  def _addRole[C <: Component](role: Role[C]): Role[C] = {
    _roles += role.name -> role
    role
  }

  override private[tcof] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)
    _roles.values.foreach(_._init(stage, config))
  }
}
