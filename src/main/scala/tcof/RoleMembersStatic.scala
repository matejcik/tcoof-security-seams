package tcof

import scala.collection.mutable
import scala.reflect.ClassTag

class RoleMembersStatic[+ComponentType <: Component](values: Iterable[ComponentType]) extends RoleMembers(values) {
  private[tcof] override def mapChildToParent(membersContainer: WithMembers[Component]): Unit = {
  }

  def select[RoleType <: Component : ClassTag]: RoleMembersStatic[RoleType] = {
    val comps = mutable.ListBuffer.empty[RoleType]

    for (value <- values) {
      value match {
        case comp: RoleType => comps += comp
        case _ =>
      }
    }

    new RoleMembersStatic[RoleType](comps)
  }

  def filter(filter: ComponentType => Boolean): RoleMembersStatic[ComponentType] = {
    val comps = mutable.ListBuffer.empty[ComponentType]

    for (value <- values) {
      if (filter(value)) {
        comps += value
      }
    }

    new RoleMembersStatic[ComponentType](comps)
  }

  def ++[B >: ComponentType <: Component](other: RoleMembersStatic[B]): RoleMembersStatic[B] =
    new RoleMembersStatic(values ++ other.values)

}
