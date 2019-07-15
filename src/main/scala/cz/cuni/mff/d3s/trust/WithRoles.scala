package cz.cuni.mff.d3s.trust

import InitStages.InitStages
import Utils._

import scala.language.implicitConversions
import scala.collection.mutable

trait WithRoles extends Initializable with CommonImplicits {
  this: WithConstraints with WithSelectionStatus =>

  private type Role[T] = MemberGroup[T]

  private[trust] val _roles = mutable.ArrayBuffer.empty[Role[Component]]

  def oneOf[C <: Component](itemFirst: C, itemRest: C*): Role[C] =
    oneOf(itemFirst +: itemRest)

  def oneOf[C <: Component](items: Iterable[C]): Role[C] =
    _addRole("oneOf_" + randomName, items, card => card === 1)

  def oneOf[C <: Component](role: Role[C]): Role[C] = subsetOf(role, _ === 1)

  def unionOf[C <: Component](roleFirst: Role[C], roleRest: Role[C]*): Role[C] =
    unionOf(roleFirst +: roleRest)

  def unionOf[C <: Component](roles: Iterable[Role[C]]): Role[C] =
    _addRole(new UnionGroup(roles), null)

  def subsetOf[C <: Component](itemFirst: C, itemRest: C*): Role[C] =
    subsetOfComponents(itemFirst +: itemRest)

  def subsetOfComponents[C <: Component](
      items: Iterable[C],
      cardinality: Integer => Logical = null,
  ): Role[C] =
    _addRole("subsetOfComponents_" + randomName, items, cardinality)

  /** XXX **/
  def subsetOf[C <: Component](
      role: Role[C],
      cardinality: Integer => Logical = null
  ): Role[C] = {
    val subsetRole =
      _addRole("subsetOfRole_" + randomName, role.allMembers, cardinality)
    constraint {
      // This block executes in RulesCreation phase, when all variables already exist
      // See definition of `constraint`
      val cons =
        _solverModel.subsetEq(subsetRole.allMembersVar, role.allMembersVar)
      LogicalBoolVar(cons.reify())
    }
    subsetRole
  }

  /** XXX **/
  def allOf[C <: Component](itemFirst: C, itemRest: C*): Role[C] =
    allOf(itemFirst +: itemRest)

  def allOf[C <: Component](items: Iterable[C]): Role[C] =
    _addRole("allOf_" + randomName, items, _ === items.size)

  def _addRole[C <: Component](
      name: String,
      items: Iterable[C],
      cardinality: Integer => Logical,
  ): Role[C] = _addRole(new Role(name, items), cardinality)

  def _addRole[C <: Component](
      role: Role[C],
      cardinality: Integer => Logical
  ): Role[C] = {
    _roles += role
    if (cardinality != null)
      constraint(cardinality.apply(role.cardinality))
    role
  }

  override private[trust] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)
    _roles.foreach(_._init(stage, config))

    stage match {
      case InitStages.RulesCreation =>
        for (role <- _roles)
          _solverModel.arithm(role.isActiveVar, "=", isSelectedVar).post()
      case _ =>
    }
  }

  implicit def componentsToRole[C <: Component](components: Iterable[C]): Role[C] =
    allOf(components)
  implicit def componentToRole[C <: Component](component: C): Role[C] =
    allOf(component)
}
