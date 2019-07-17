package cz.cuni.mff.d3s.trust

import scala.language.implicitConversions
import org.chocosolver.solver.variables.SetVar

import scala.collection.mutable

/** Miscellaneous implicit conversions and extension methods.
  *
  * Provides an implicit conversion from `Int` to [[Integer]], from `Boolean` to
  * [[Logical]], and adds constraint methods to iterables of `MemberGroup`s.
  */
trait CommonImplicits {
  this: Initializable =>

  /** Extension methods for iterables of MemberGroups.
    *
    * Useful when working with collections of roles from sub-ensembles:
    * {{{
    *   class ExampleEnsemble extends Ensemble {
    *     val role = subsetOf(/*...*/)
    *   }
    *
    *   val collection = rules(exampleEnsembles)
    *   val roles = collection.map(_.role)
    *   constraint { roles.allDisjoint }
    * }}}
    *
    * @param memberGroups Member group collection being extended
    */
  implicit class WithMembersIterable(memberGroups: Iterable[MemberGroup[_]]) {

    /** State that all groups in the collection must be disjoint.
      *
      * More specifically, the same member object is allowed to be a member of at most
      * one of the groups in the collection.
      *
      * If the collection is empty, the constraint is satisfied automatically.
      *
      * @return a constraint that is satisfied if all groups in the collection are disjoint.
      */
    def allDisjoint: Logical =
      if (memberGroups.isEmpty) LogicalBoolean(true)
      else {
        // assign global indices over the set of all members of all groups
        val allMembersIndices = memberGroups.flatMap(_.allMembers).toSet.zipWithIndex.toMap
        val allMembersVars = mutable.ListBuffer.empty[SetVar]

        for (group <- memberGroups) {
          // create a SetVar for each group, using the global indices for membership
          val allMembersVar = _solverModel.setVar(
            "AD_" + group.allMembersVarName,
            Array.empty[Int],
            allMembersIndices.values.toArray
          )
          allMembersVars += allMembersVar

          // link group membership to membership in the new SetVar
          for ((member, memberIdx) <- group.allMembers.zipWithIndex) {
            val idxInAllMembersVar = allMembersIndices(member)
            _solverModel.ifOnlyIf(
              _solverModel.member(memberIdx, group.allMembersVar),
              _solverModel.member(idxInAllMembersVar, allMembersVar)
            )
          }
        }

        // generate an allDisjoint constraint over the newly created SetVars, ensuring
        // that no global index appears in more than one of the sets
        LogicalBoolVar(_solverModel.allDisjoint(allMembersVars: _*).reify())
      }

    /** Total cardinality of the groups in the collection.
      *
      * @return a cardinality variable representing the sum of cardinalities of all
      *         groups in the collection
      */
    def cardinality: Integer = memberGroups.map(_.cardinality).reduceOption(_ + _).getOrElse(0)
  }

  /** Convert native Boolean to Logical object.
    *
    * This enables the following usage:
    * {{{
    *   constraint {
    *     someRoles.allDisjoint && someCollection.notEmpty
    *   }
    * }}}
    */
  implicit def booleanToLogical(x: Boolean): LogicalBoolean = LogicalBoolean(x)

  /** Convert native Int to Integer object.
    *
    * This enables the following usage:
    * {{{
    *   constraint {
    *     someRoles.cardinality + 5 > otherRoles.cardinality * 7
    *   }
    * }}}
    */
  implicit def intToInteger(value: Int): Integer = _solverModel.IntegerInt(value)
}
