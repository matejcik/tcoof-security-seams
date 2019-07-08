package external

import cz.cuni.mff.d3s.enact.{Ensemble, Policy}

import scala.language.reflectiveCalls

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
      name("root")
      object SubEnsemble extends Ensemble {
        name("sub")
        situation { subEnabled }

        val role = subsetOf(members, _ > 20)
      }

      rules(SubEnsemble)
    }

    val rootOk = Policy.root(new EnsembleWithSubensemble(false))
    val rootFail = Policy.root(new EnsembleWithSubensemble(true))

    assert(rootOk.resolve())
    assert(!rootFail.resolve())
  }

  "allOf" should "select all components" in {
    val members = for (i <- 1 to 5) yield Member(i)

    val policy = Policy.root(new Ensemble {
      val role = allOf(members)
    })

    assert(policy.resolve())
    policy.instance.role.selectedMembers.size shouldEqual members.size
  }

  it should "group components specified one by one" in {
    val policy = Policy.root(new Ensemble {
      val members = allOf(Member(1), Member(2), Member(3))
    })

    assert(policy.resolve())
    policy.instance.members.selectedMembers should have size 3
  }
}
