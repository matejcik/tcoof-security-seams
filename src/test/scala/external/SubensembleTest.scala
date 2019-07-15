package external

import cz.cuni.mff.d3s.trust._

import scala.language.reflectiveCalls

class SubensembleTest extends TestClass {
  "multiple subensembles" should "behave identically with multiple rules calls" in {
    val members = for (i <- 1 to 10) yield Member(i)

  }

  "subensemble" should "not select members" in {
    val members = for (i <- 1 to 10) yield Member(i)
    val problem = Policy.root(new Ensemble {

      class Ens extends Ensemble {
        situation { false }
        val x = subsetOf(members, _ > 1)
      }

      val ens = new Ens
      rules(ens)
    })

    assert(problem.resolve())
    problem.instance.ens.x.selectedMembers shouldBe empty
  }

  "active ensemble" should "allow empty roles" in {
    val members = for (i <- 1 to 10) yield Member(i)
    val problem = Policy.root(new Ensemble {

      class Ens extends Ensemble {
        situation { true }
        val x = subsetOf(members, _ === 0)
      }

      val ens = new Ens
      rules(ens)
    })

    assert(problem.resolve())
  }
}
