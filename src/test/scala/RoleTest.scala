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

  "allOf" should "select all components" in {
    val members = for (i <- 1 to 5) yield Member(i)

    val policy = Scenario.root(new Ensemble {
      val role = allOf(members)
    })

    assert(policy.resolve())
    policy.instance.role.selectedMembers.size shouldEqual members.size
  }

  it should "group components specified one by one" in {
    val policy = Scenario.root(new Ensemble {
      val members = allOf(Member(1), Member(2), Member(3))
    })

    assert(policy.resolve())
    policy.instance.members.selectedMembers should have size 3
  }
}
