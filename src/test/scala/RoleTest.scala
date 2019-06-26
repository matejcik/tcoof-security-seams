import scala.language.reflectiveCalls

import tcof._

class RoleTest extends ModelSolver {
  "roles" should "only accept components" in {
    case class NotAComponent(id: Int)
    val members = for (i <- 1 to 5) yield NotAComponent(i)

    val root = new Ensemble {}
    "root.oneOf(members)" shouldNot typeCheck
  }
}
