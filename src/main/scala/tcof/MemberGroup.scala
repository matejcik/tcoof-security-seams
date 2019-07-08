package tcof

import org.chocosolver.solver.variables.{BoolVar, SetVar}
import tcof.InitStages.InitStages

import scala.reflect.ClassTag

abstract class MemberGroup[+MemberType](
    values: Iterable[MemberType]
) extends Initializable
    with CommonImplicits {

  private[tcof] def allMembersVarName: String

  private[tcof] val allMembers: IndexedSeq[MemberType] = values.toSet.toIndexedSeq

  private[tcof] var allMembersVar: SetVar = _
  private[tcof] var isActiveVar: BoolVar = _

  override private[tcof] def _init(stage: InitStages, config: Config): Unit = {
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

  def _channelMapResults[T](fun: MemberType => T,
                            valMap: Map[T, Int]): SetVar = {
    val memberMap = allMembers.indices.groupBy(idx => fun(allMembers(idx)))
    val channelVar = _solverModel.setVar(
      Array.empty[Int],
      memberMap.keys.map(valMap(_)).toArray)
    for ((value, indices) <- memberMap) {
      val memberships = for (idx <- indices)
        yield _solverModel.member(idx, allMembersVar)
      val or = _solverModel.or(memberships: _*)
      _solverModel.ifOnlyIf(or, _solverModel.member(valMap(value), channelVar))
    }
    channelVar
  }

  def allEqual[T](fun: MemberType => T): Logical = {
    val values = allMembers.map(fun).toSet.zipWithIndex.toMap
    val channelVar = _channelMapResults(fun, values)
    _solverModel.IntegerIntVar(channelVar.getCard) === 1
  }

  def allDifferent[T](fun: MemberType => T): Logical = {
    val values = allMembers.map(fun).toSet.zipWithIndex.toMap
    val channelVar = _channelMapResults(fun, values)
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
  }

  def membersWithSelectionIndicator: Iterable[(Boolean, MemberType)] = {
    val selection = _solverModel.solution.getSetVal(allMembersVar)
    allMembers.zipWithIndex.map {
      case (member, idx) => (selection.contains(idx), member)
    }
  }*/

  def selectedMembers: Iterable[MemberType] = {
    for (idx <- _solverModel.solution.getSetVal(allMembersVar))
      yield allMembers(idx)
  }
}
