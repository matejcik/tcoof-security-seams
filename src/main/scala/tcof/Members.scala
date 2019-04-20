package tcof
import scala.reflect.ClassTag

/** Represents a set of potential members (e.g. components that are to be used for a role, sub-ensembles of an ensembles).
  * In case the selection of members is dependent on the parent, it is specialized by MembersFromParent. In case the members
  * come from the universe (i.e. are not conditioned by existence in a parent), they are specialized as MembersFromUniverse.
  */
abstract class Members[+MemberType](
    private[tcof] val values: Iterable[MemberType]
) {
  private[tcof] def size = values.size

  def map[O: ClassTag](fun: MemberType => O): Iterable[O] = values map fun
  def flatMap[O: ClassTag](fun: MemberType => O): Iterable[O] = values map fun
}
