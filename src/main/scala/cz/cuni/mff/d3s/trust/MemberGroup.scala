package cz.cuni.mff.d3s.trust

import org.chocosolver.solver.variables.{BoolVar, SetVar}
import InitStages.InitStages
import Utils._
import org.chocosolver.solver.constraints.nary.cnf.LogOp

import scala.reflect.ClassTag

class MemberGroup[+MemberType](
    val name: String,
    values: Iterable[MemberType]
) extends Initializable
    with CommonImplicits {

  private[trust] val allMembersVarName: String = "G_" + randomName
  private[trust] val allMembers: IndexedSeq[MemberType] = values.toSet.toIndexedSeq

  private[trust] var allMembersVar: SetVar = _
  private[trust] var isActiveVar: BoolVar = _

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

  def cardinality: Integer = _solverModel.IntegerIntVar(allMembersVar.getCard)

  def contains(member: Any): Logical = {
    val idx = allMembers.indexOf(member)
    if (idx == -1) LogicalBoolean(false)
    else LogicalBoolVar(_solverModel.member(idx, allMembersVar).reify())
  }

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

  def containsOnly(member: Any): Logical = {
    val idx = allMembers.indexOf(member)
    if (idx == -1) LogicalBoolean(false)
    else cardinality === 1 && LogicalBoolVar(_solverModel.member(idx, allMembersVar).reify())
  }

  def sum(func: MemberType => Integer): Integer =
    _solverModel.sumBasedOnMembership(allMembersVar, allMembers.map(func))

  def all(func: MemberType => Logical): Logical =
    _solverModel.forAllSelected(allMembers.map(func), allMembersVar)

  def some(func: MemberType => Logical): Logical =
    _solverModel.existsSelected(allMembers.map(func), allMembersVar)

  def disjointAfterMap[OtherMemberType, T: ClassTag](
      funcThis: MemberType => T,
      other: MemberGroup[OtherMemberType],
      funcOther: OtherMemberType => T
  ): Logical = {
    val thisValues = allMembers.map(funcThis)
    val otherValues = other.allMembers.map(funcOther)

    val allMap = thisValues.toSet.union(otherValues.toSet).zipWithIndex.toMap

    val thisVar =
      _solverModel.setVar(Array.empty[Int], thisValues.map(allMap(_)).toArray)
    val otherVar =
      _solverModel.setVar(Array.empty[Int], otherValues.map(allMap(_)).toArray)

    val thisMembers = thisValues.zipWithIndex
    for ((member, idx) <- thisMembers) {
      _solverModel.ifOnlyIf(
        _solverModel.member(idx, allMembersVar),
        _solverModel.member(allMap(member), thisVar)
      )
    }

    val otherMembers = otherValues.zipWithIndex
    for ((member, idx) <- otherMembers) {
      _solverModel.ifOnlyIf(
        _solverModel.member(idx, other.allMembersVar),
        _solverModel.member(allMap(member), otherVar)
      )
    }

    LogicalBoolVar(_solverModel.disjoint(thisVar, otherVar).reify())
  }

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

  def allEqual[T](func: MemberType => T): Logical = {
    val values = allMembers.map(func).toSet.zipWithIndex.toMap
    val channelVar = _channelMapResults(func, values)
    _solverModel.IntegerIntVar(channelVar.getCard) <= 1
  }

  def allDifferent[T](func: MemberType => T): Logical = {
    val values = allMembers.map(func).toSet.zipWithIndex.toMap
    val channelVar = _channelMapResults(func, values)
    _solverModel.IntegerIntVar(channelVar.getCard) === cardinality
  }

  /* TODO remove this?
  def foreachBySelection(
      forSelected: MemberType => Unit,
      forNotSelected: MemberType => Unit
  ): Unit = {
    val selection = _solverModel.solution.getSetVal(allMembersVar)
    for ((member, idx) <- allMembers.zipWithIndex) {
      if (selection.contains(idx))
        forSelected(member)
      else
        forNotSelected(member)
    }
  }*/

  def membersWithSelectionIndicator: Iterable[(Boolean, MemberType)] = {
    val selection = _solverModel.solution.getSetVal(allMembersVar)
    allMembers.zipWithIndex.map { case (member, idx) => (selection.contains(idx), member) }
  }

  def selectedMembers: Iterable[MemberType] =
    for (idx <- _solverModel.solution.getSetVal(allMembersVar))
      yield allMembers(idx)

  override def toString: String = s"<MemberGroup:$allMembersVarName:$name>"
}
