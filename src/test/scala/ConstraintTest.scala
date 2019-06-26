import scala.language.reflectiveCalls
import tcof._

class ConstraintTest extends ModelSolver {
  "allEqual" should "ensure all are equal" in {
    val members = for (_ <- 1 to 5; i <- 1 to 5) yield Member(i)
    println(members)

    val problem = Scenario.root(new Ensemble {
      val selected = subsetOf(members, _ === 5)

      constraint { selected.allEqual(_.id) }
    })

    problem.init()
    var solutions = 0
    while (problem.solve()) {
      val ids = problem.instance.selected.selectedMembers.map(_.id)
      ids should have size 5
      assert(ids.forall(_ == ids.head))
      solutions += 1
    }

    solutions shouldBe 5
  }
}
