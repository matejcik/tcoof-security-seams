package tcof

import org.chocosolver.solver.variables.SetVar

import scala.collection.mutable

trait CommonImplicits {
  this: WithConfig =>

  implicit class WithMembersIterable[MemberType](
      memberGroups: Iterable[WithMembers[MemberType]]
  ) {
    def allDisjoint: Logical =
      if (memberGroups.isEmpty)
        LogicalBoolean(true)
      else {
        val allMembersIndices =
          memberGroups.flatMap(_.allMembers).toSet.zipWithIndex.toMap
        val allMembersVars = mutable.ListBuffer.empty[SetVar]

        for (group <- memberGroups) {
          val allMembersVar = _solverModel.setVar(
            "AD_" + group.allMembersVarName,
            Array.empty[Int],
            allMembersIndices.values.toArray
          )
          allMembersVars += allMembersVar

          for ((member, memberIdx) <- group.allMembers.zipWithIndex) {
            val idxInAllMembersVar = allMembersIndices(member)
            _solverModel.ifOnlyIf(
              _solverModel.member(memberIdx, group.allMembersVar),
              _solverModel.member(idxInAllMembersVar, allMembersVar)
            )
          }
        }

        LogicalBoolVar(_solverModel.allDisjoint(allMembersVars: _*).reify())
      }
  }

  implicit def booleanToLogical(x: Boolean): LogicalBoolean = LogicalBoolean(x)
  implicit def intToInteger(value: Int): Integer =
    _solverModel.IntegerInt(value)

}
