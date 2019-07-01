package tcof

import tcof.InitStages.InitStages
import tcof.Utils._

import scala.collection.mutable

trait WithRoles extends Initializable with CommonImplicits {

  private[tcof] val _roles = mutable.Map.empty[String, Role[Component]]

  def oneOf[C <: Component](itemFirst: C, itemRest: C*): Role[C] =
    oneOf(itemFirst +: itemRest)

  def oneOf[C <: Component](items: Iterable[C]): Role[C] =
    _addRole("oneOf_" + randomName, items, card => card === 1)

  def unionOf[C <: Component](roleFirst: Role[C], roleRest: Role[C]*): Role[C] =
    unionOf(roleFirst +: roleRest)

  def unionOf[C <: Component](roles: Iterable[Role[C]]): Role[C] =
    _addRole(new UnionRole("unionOf_" + randomName, roles))

  def subsetOf[C <: Component](
      items: Iterable[C],
      cardinality: Integer => Logical = null,
  ): Role[C] =
    _addRole("subsetOf_" + randomName, items, cardinality)

  def subsetOf[C <: Component](itemFirst: C, itemRest: C*): Role[C] =
    subsetOf(itemFirst +: itemRest)

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

  implicit def roleToComponents[C <: Component](role: Role[C]): Iterable[C] =
    role.selectedMembers

  implicit def componentsToRole[C <: Component](
      components: Iterable[C]): Role[C] =
    subsetOf(components, _ === components.size)

  implicit def componentToRole[C <: Component](component: C): Role[C] =
    componentsToRole(Seq(component))
}
