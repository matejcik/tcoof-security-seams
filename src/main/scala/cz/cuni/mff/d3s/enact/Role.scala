package cz.cuni.mff.d3s.enact

/** Represents a role in an ensemble. Implements methods to build membership over components contained in a role. */
class Role[+ComponentType <: Component](
    val name: String,
    values: Iterable[ComponentType],
) extends MemberGroup(values)
    with Initializable {

  override def allMembersVarName: String = "R_" + name

  override def toString: String =
    s"""Role "$name""""
//    s"""Role "$name": ${selectedMembers.map(_.toString).mkString(" ")}\n"""
}
