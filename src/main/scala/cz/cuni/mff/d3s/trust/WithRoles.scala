package cz.cuni.mff.d3s.trust

import InitStages.InitStages
import Utils._

import scala.language.implicitConversions
import scala.collection.mutable

/** Ensemble part for handling roles.
  *
  * Provides DSL functions that create different kinds of roles.
  */
trait WithRoles extends Initializable with CommonImplicits {
  this: WithConstraints with WithSelectionStatus =>

  /** Local type alias for roles. */
  private type Role[T] = MemberGroup[T]

  /** List of role objects. */
  private val _roles = mutable.ArrayBuffer.empty[Role[Component]]

  /** Create a role inhabited by one of the components.
    *
    * @param itemFirst Mandatory component
    * @param itemRest Optional other components
    * @tparam C Concrete type of component
    * @return a role object that selects one of the candidates.
    */
  def oneOf[C <: Component](itemFirst: C, itemRest: C*): Role[C] =
    oneOf(itemFirst +: itemRest)

  /** Create a role inhabited by one of the components.
    *
    * @param items Candidate components
    * @tparam C Concrete type of component
    * @return a role object that selects one of the candidates.
    */
  def oneOf[C <: Component](items: Iterable[C]): Role[C] =
    _addRole("oneOf_" + randomName, items, card => card === 1)

  /** Create a role inhabited by one of the members of the other role.
    *
    * @param role Other role
    * @tparam C Concrete type of component
    * @return a role object that selects one of the candidates.
    */
  def oneOf[C <: Component](role: Role[C]): Role[C] =
    subsetOf(role, _ === 1)

  /** Create a union of roles.
    *
    * @param roleFirst Mandatory first role
    * @param roleRest Optional other roles
    * @tparam C Concrete type of component
    * @return a role object inhabited by the union of inhabitants of the constituent roles
    */
  def unionOf[C <: Component](roleFirst: Role[C], roleRest: Role[C]*): Role[C] =
    unionOf(roleFirst +: roleRest)

  /** Create a union of roles.
    *
    * @param roles Constituent roles
    * @tparam C Concrete type of component
    * @return a role object inhabited by the union of inhabitants of the constituent roles
    */
  def unionOf[C <: Component](roles: Iterable[Role[C]]): Role[C] =
    _addRole(new UnionGroup(roles), null)

  /** Create a role inhabited by a subset of the components.
    *
    * @param itemFirst Mandatory first component
    * @param itemRest Optional other components
    * @tparam C Concrete type of component
    * @return a role inhabited by some subset of the components.
    */
  def subsetOf[C <: Component](itemFirst: C, itemRest: C*): Role[C] =
    subsetOfComponents(itemFirst +: itemRest)

  /** Create a role inhabited by a subset of the components.
    *
    * It is possible to specify a cardinality constraint -- a function that takes
    * the subset cardinality as an argument and returns a `Logical` constraint.
    * E.g.:
    * {{{
    *   val role = subsetOf(members, _ > 5)
    * }}}
    *
    * This variant exists to optimize the `subsetOf(Role)` call. It cannot be a direct
    * overload -- Scala allows it, but IDEA bug https://youtrack.jetbrains.com/issue/SCL-15793
    * makes it very uncomfortable to work with the source code afterwards.
    *
    * @param items Candidate components
    * @param cardinality Optionally specified cardinality constraint
    * @tparam C Concrete type of component
    * @return a role inhabited by some subset of the components.
    */
  def subsetOfComponents[C <: Component](
      items: Iterable[C],
      cardinality: Integer => Logical = null,
  ): Role[C] =
    _addRole("subsetOfComponents_" + randomName, items, cardinality)

  /** Create a role inhabited by a subset of members of a different role.
    *
    * It is possible to specify a cardinality constraint -- a function that takes
    * the subset cardinality as an argument and returns a `Logical` constraint.
    * E.g.:
    * {{{
    *   val role = subsetOf(members, _ > 5)
    * }}}
    *
    * @param role Other role
    * @param cardinality Optionally specified cardinality constraint
    * @tparam C Concrete type of component
    * @return a role inhabited by some subset of the other role
    */
  def subsetOf[C <: Component](
      role: Role[C],
      cardinality: Integer => Logical = null
  ): Role[C] = {
    val subsetRole = _addRole("subsetOfRole_" + randomName, role.allMembers, cardinality)
    constraint {
      // This block executes in RulesCreation phase, when all variables already exist
      // See definition of `constraint`
      val cons = _solverModel.subsetEq(subsetRole.allMembersVar, role.allMembersVar)
      LogicalBoolVar(cons.reify())
    }
    subsetRole
  }

  /** Create a role inhabited by all of the components.
    *
    * @param itemFirst Mandatory component
    * @param itemRest Optional other components
    * @tparam C Concrete type of component
    * @return a role object that selects all of the candidates.
    */
  def allOf[C <: Component](itemFirst: C, itemRest: C*): Role[C] =
    allOf(itemFirst +: itemRest)

  /** Create a role inhabited by all of the components.
    *
    * It is usually not necessary to use this function, as the implicit conversion
    * [[componentsToRole]] performs this action. This function exists for parity
    * with `oneOf` and `subsetOf`, and for cases where the implicit conversion
    * cannot be used.
    *
    * @param items Candidate components
    * @tparam C Concrete type of component
    * @return a role object that selects all of the candidates.
    */
  def allOf[C <: Component](items: Iterable[C]): Role[C] =
    _addRole("allOf_" + randomName, items, _ === items.toSet.size)

  /** Register a role from a list of components
    *
    * @param name Name of the role
    * @param items List of candidates
    * @param cardinality Cardinality constraint
    * @tparam C Concrete type of component
    * @return the new role object
    */
  private def _addRole[C <: Component](
      name: String,
      items: Iterable[C],
      cardinality: Integer => Logical,
  ): Role[C] = _addRole(new Role(name, items), cardinality)

  /** Register a role
    *
    * Adds the role object to the internal list, and installs its cardinality constraint
    * to the list of this ensemble's constraints.
    *
    * @param role The new role object
    * @param cardinality Cardinality constraint
    * @tparam C Concrete type of component
    * @return the same role object.
    */
  private def _addRole[C <: Component](
      role: Role[C],
      cardinality: Integer => Logical
  ): Role[C] = {
    _roles += role
    if (cardinality != null) constraint(cardinality.apply(role.cardinality))
    role
  }

  /** Initialization hook.
    *
    * Propagates [[_init()]] call to registered roles and installs a dependency of the
    * role object activation on the selection status of this ensemble.
    */
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

  /** Convert a list of components to a role selecting all of them. */
  implicit def componentsToRole[C <: Component](components: Iterable[C]): Role[C] =
    allOf(components)

  /** Convert a single component to a role selecting it. */
  implicit def componentToRole[C <: Component](component: C): Role[C] =
    allOf(component)
}
