package external

import cz.cuni.mff.d3s.enact._

import scala.language.reflectiveCalls

class ConstraintTest extends TestClass {
  "allEqual" should "ensure all are equal" in {
    val members = for (i <- 1 to 25) yield Member(i)

    val problem = Policy.root(new Ensemble {
      val selection = subsetOf(members, _ === 5)

      constraint { selection.allEqual(_.id % 5) }
    })

    problem.init()
    var solutions = 0
    while (problem.solve()) {
      val ids = problem.instance.selection.selectedMembers.map(_.id)
      ids should have size 5
      assert(ids.forall(_ % 5 == ids.head % 5))
      solutions += 1
    }

    solutions shouldBe 5
  }

  it should "work with empty roles" in {
    val problem = Policy.root(new Ensemble {
      val emptyList = Seq.empty[Component]
      val role = subsetOf(emptyList)
      constraint { role.allEqual(x => x) }
    })
  }
}
