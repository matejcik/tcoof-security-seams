import scala.language.reflectiveCalls
import org.scalatest.{FlatSpec, Matchers}
import tcof._

class RoleTest extends FlatSpec with ModelSolver with Matchers {
  "roles" should "only accept components" in {
    case class NotAComponent(id: Int)
    val members = for (i <- 1 to 5) yield NotAComponent(i)

    val root = new RootEnsemble {}
    "root.oneOf(members)" shouldNot typeCheck
  }
}
