import scala.language.reflectiveCalls
import tcof._

class RoleTest extends ModelSolver {
  "role" should "only accept components" in {
    case class NotAComponent(id: Int)
    val members = for (i <- 1 to 5) yield NotAComponent(i)

    val root = new Ensemble {}
    "root.oneOf(members)" shouldNot typeCheck
  }

  it should "ignore constraints in inactive ensembles" in {
    val members = for (i <- 1 to 5) yield Member(i)

    class EnsembleWithSubensemble(subEnabled: Boolean) extends Ensemble {
      object SubEnsemble extends Ensemble {
        situation { subEnabled }

        val role = subsetOf(members, _ > 20)
      }

      rules(SubEnsemble)
    }

    val rootOk = Scenario.root(new EnsembleWithSubensemble(false))
    val rootFail = Scenario.root(new EnsembleWithSubensemble(true))

    assert(rootOk.resolve())
    assert(!rootFail.resolve())
  }
}
