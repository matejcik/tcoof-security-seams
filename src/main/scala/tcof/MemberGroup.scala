package tcof

import org.chocosolver.solver.variables.SetVar
import tcof.InitStages.InitStages

import scala.reflect.ClassTag

abstract class MemberGroup[+MemberType](
    val values: Iterable[MemberType]
) extends WithConfig {

  private[tcof] def allMembersVarName: String

  private[tcof] val allMembers: IndexedSeq[MemberType] = values.toIndexedSeq

  private[tcof] var allMembersVar: SetVar = null

  override private[tcof] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)

    stage match {
      case InitStages.VarsCreation =>
        allMembersVar = _solverModel.setVar(
          allMembersVarName,
          Array.empty[Int],
          allMembers.indices.toArray,
        )
      case _ =>
    }
  }

  def cardinality: Integer = _solverModel.IntegerIntVar(allMembersVar.getCard)

  def contains(member: Any): Logical = some(x => LogicalBoolean(x == member))
  def containsOtherThan(member: Any): Logical =
    some(x => LogicalBoolean(x != member))
  def containsOnly(member: Any): Logical = all(x => LogicalBoolean(x == member))

  def sum(fun: MemberType => Integer): Integer =
    _solverModel.sumBasedOnMembership(allMembersVar, allMembers.map(fun))

  def all(fun: MemberType => Logical): Logical =
    _solverModel.forAllSelected(allMembers.map(fun), allMembersVar)

  def some(fun: MemberType => Logical): Logical =
    _solverModel.existsSelected(allMembers.map(fun), allMembersVar)

  def disjointAfterMap[OtherMemberType, T: ClassTag](
      funThis: MemberType => T,
      other: MemberGroup[OtherMemberType],
      funOther: OtherMemberType => T
  ): Logical = {
    val thisValues = allMembers.map(funThis)
    val otherValues = other.allMembers.map(funOther)

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
  }

  def allEqual(fun: MemberType => Any): Logical = {
    val values = allMembers.map(fun).toIndexedSeq
    val valueIndexMap = values.toSet.zipWithIndex.toMap
    val valueIntVars = values.map(v => _solverModel.intVar(valueIndexMap(v)))
    val namePrefix = "Ch_" + Utils.randomName + "_"

    val sets = for (i <- valueIndexMap.values)
      yield
        _solverModel.setVar(
          namePrefix + i,
          Array.emptyIntArray,
          allMembers.indices.toArray
        )

    _solverModel.setsIntsChanneling(sets.toArray, valueIntVars.toArray).post
    val subsetConstraints = for (set <- sets)
      yield _solverModel.subsetEq(allMembersVar, set)
    LogicalBoolVar(_solverModel.or(subsetConstraints.toSeq: _*).reify())
  }

  def membersWithSelectionIndicator: Iterable[(Boolean, MemberType)] = {
    val selection = _solverModel.solution.getSetVal(allMembersVar)
    allMembers.zipWithIndex.map {
      case (member, idx) => (selection.contains(idx), member)
    }
  }

  def selectedMembers: Iterable[MemberType] = {
    for (idx <- _solverModel.solution.getSetVal(allMembersVar))
      yield allMembers(idx)
  }
}
