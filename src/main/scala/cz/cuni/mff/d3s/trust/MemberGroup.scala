package cz.cuni.mff.d3s.trust

import org.chocosolver.solver.variables.{BoolVar, SetVar}
import InitStages.InitStages
import Utils._
import org.chocosolver.solver.constraints.nary.cnf.LogOp

import scala.reflect.ClassTag

/** Group of candidates for selection by the solver.
  *
  * Represents a collection underpinned by a [[SetVar]]. The solver can determine
  * whether any of the `values` should be selected in the solution.
  *
  * See thesis section 6.1.7 for details.
  *
  * @param name Name of the group
  * @param values Collection of candidate instances
  * @tparam MemberType Concrete type of the candidates
  */
class MemberGroup[+MemberType](val name: String, values: Iterable[MemberType])
    extends Initializable
    with CommonImplicits {

  /** Name of the underlying `SetVar` */
  private[trust] val allMembersVarName: String = "G_" + randomName

  /** Collection of member instances with stable integer indices. */
  private[trust] val allMembers: IndexedSeq[MemberType] = values.toSet.toIndexedSeq

  /** Solver variable representing the membership solution. */
  private[trust] var allMembersVar: SetVar = _

  /** Solver variable representing the activation status.
    *
    * If the group is inactive, it selects no members.
    */
  private[trust] var isActiveVar: BoolVar = _

  /** Initialization hook.
    *
    * Propagates [[_init()]] call to members, creates solver variables, and installs
    * a rule to exclude all members if the group is inactive.
    */
  override private[trust] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)

    stage match {
      case InitStages.VarsCreation => {
        allMembersVar = _solverModel.setVar(
          allMembersVarName,
          Array.empty[Int],
          allMembers.indices.toArray,
        )
        isActiveVar = _solverModel.boolVar()
      }
      case InitStages.RulesCreation => {
        _solverModel.ifThen(
          isActiveVar.not().asInstanceOf[BoolVar],
          _solverModel.not(_solverModel.notEmpty(allMembersVar))
        )
      }
      case _ =>
    }
  }

  /** Cardinality of the selected members set. */
  def cardinality: Integer = _solverModel.IntegerIntVar(allMembersVar.getCard)

  /** State that the group contains the given member.
    *
    * @param member Any object
    * @return a constraint is satisfied if `member` is selected in this group.
    */
  def contains(member: Any): Logical = {
    val idx = allMembers.indexOf(member)
    if (idx == -1) LogicalBoolean(false)
    else LogicalBoolVar(_solverModel.member(idx, allMembersVar).reify())
  }

  /** State that the group contains a member different from the given one.
    *
    * @param member Any object
    * @return a constraint is satisfied if there is at least one member selected
    *         other than `member`.
    */
  def containsOtherThan(member: Any): Logical = {
    val idx = allMembers.indexOf(member)
    val atLeastOne = cardinality > 0
    if (idx == -1) atLeastOne
    else {
      val contains = _solverModel.member(idx, allMembersVar).reify()
      val atLeastTwo = _solverModel.arithm(allMembersVar.getCard, ">", 1).reify()
      atLeastOne && LogicalLogOp(LogOp.implies(contains, atLeastTwo))
    }
  }

  /** State that the given member is the only selected member of the group.
    *
    * @param member Any object
    * @return a constraint satisfied if `member` is the only selected member in
    *         the group.
    */
  def containsOnly(member: Any): Logical = {
    val idx = allMembers.indexOf(member)
    if (idx == -1) LogicalBoolean(false)
    else cardinality === 1 && LogicalBoolVar(_solverModel.member(idx, allMembersVar).reify())
  }

  /** Sum of results over selected members of the group.
    *
    * @param func Mapping function
    * @return the sum of results of `func` applied to all selected members.
    */
  def sum(func: MemberType => Integer): Integer =
    _solverModel.sumBasedOnMembership(allMembersVar, allMembers.map(func))

  /** State that a predicate is true for all selected members.
    *
    * @param func Predicate
    * @return a constraint satisfied if `func` is true for all selected members.
    */
  def all(func: MemberType => Logical): Logical =
    _solverModel.forAllSelected(allMembers.map(func), allMembersVar)

  /** State that a predicate is true for at least one selected member.
    *
    * @param func Predicate
    * @return a constraint satisfied if `func` is true for at least one selected member.
    */
  def some(func: MemberType => Logical): Logical =
    _solverModel.existsSelected(allMembers.map(func), allMembersVar)

  /** Create a set of values linked to members of the group.
    *
    * Creates a [[SetVar]] whose members correspond to distinct results of `func` over
    * the group members. A value is a member of the channeling iff at least one selected
    * member maps to that value.
    *
    * @param func Mapping function
    * @param valMap Collection of possible values
    * @tparam T Value type
    * @return a [[SetVar]] channeling the mapping function.
    */
  def _channelMapResults[T](func: MemberType => T, valMap: Map[T, Int]): SetVar = {
    val memberMap = allMembers.indices.groupBy(idx => func(allMembers(idx)))
    val channelVar = _solverModel.setVar(Array.empty[Int], memberMap.keys.map(valMap(_)).toArray)
    for ((value, indices) <- memberMap) {
      val memberships = for (idx <- indices)
        yield _solverModel.member(idx, allMembersVar)
      val or = _solverModel.or(memberships: _*)
      _solverModel.ifOnlyIf(or, _solverModel.member(valMap(value), channelVar))
    }
    channelVar
  }

  /** State that results of mapping are disjoint.
    *
    * Maps this group to a collection of values with `funcThis`, and maps the `other`
    * group to a collection of values of the same type with `funcOther`. The resulting
    * constraint is satisfied if the resulting sets of values are disjoint.
    *
    * @param funcThis Mapping function from this group
    * @param other Other group
    * @param funcOther Mapping function from other group
    * @tparam OtherMemberType Type of other group members
    * @tparam T Common result type of mapping functions
    * @return a constraint satisfied if results of the mapping are disjoint.
    */
  def disjointAfterMap[OtherMemberType, T: ClassTag](
      funcThis: MemberType => T,
      other: MemberGroup[OtherMemberType],
      funcOther: OtherMemberType => T
  ): Logical = {
    val thisValues = allMembers.map(funcThis)
    val otherValues = other.allMembers.map(funcOther)

    val allMap = (thisValues ++ otherValues).toSet.zipWithIndex.toMap
    var thisChannel = _channelMapResults(funcThis, allMap)
    val otherChannel = other._channelMapResults(funcOther, allMap)

    LogicalBoolVar(_solverModel.disjoint(thisChannel, otherChannel).reify())
  }

  /** State that results of mapping are identical.
    *
    * @param func Mapping
    * @tparam T Result type
    * @return a constraint satisfied if all selected members map to the same value.
    */
  def allEqual[T](func: MemberType => T): Logical = {
    val values = allMembers.map(func).toSet.zipWithIndex.toMap
    val channelVar = _channelMapResults(func, values)
    _solverModel.IntegerIntVar(channelVar.getCard) <= 1
  }

  /** State that results of mapping are distinct.
    *
    * @param func Mapping
    * @tparam T Result type
    * @return a constraint satisfied if every selected member maps to a different value.
    */
  def allDifferent[T](func: MemberType => T): Logical = {
    val values = allMembers.map(func).toSet.zipWithIndex.toMap
    val channelVar = _channelMapResults(func, values)
    _solverModel.IntegerIntVar(channelVar.getCard) === cardinality
  }

  /** Iterator over members with a selection indicator.
    *
    * Can only be used if a solution exists.
    *
    * @return a list of tuples, where the first item is true if the second item is
    *         a selected member.
    */
  def membersWithSelectionIndicator: Iterable[(Boolean, MemberType)] = {
    val selection = _solverModel.solution.getSetVal(allMembersVar)
    allMembers.zipWithIndex.map { case (member, idx) => (selection.contains(idx), member) }
  }

  /** Collection of selected members.
    *
    * Can only be used if a solution exists.
    *
    * @return a collection of those member instances that are selected in the solution.
    */
  def selectedMembers: Iterable[MemberType] =
    for (idx <- _solverModel.solution.getSetVal(allMembersVar))
      yield allMembers(idx)

  override def toString: String = s"<MemberGroup:$allMembersVarName:$name>"
}
