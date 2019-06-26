import scala.language.reflectiveCalls
import tcof._

class UtilityTest extends ModelSolver {
  "utility" should "change result of solving" in {
    val members = for (i <- 1 to 5) yield Member(i)

    val noUtilitySolution = Scenario.root(new Ensemble {
      val member = oneOf(members)
    })
    val utilitySolution = Scenario.root(new Ensemble {
      val member = oneOf(members)

      utility { member.sum(_.id) }
    })

    noUtilitySolution.resolve()
    utilitySolution.resolve()

    noUtilitySolution.instance.member.selectedMembers.head.id shouldBe 1
    utilitySolution.instance.member.selectedMembers.head.id shouldBe 5
  }

  it should "be additive" in {
    val members = for (i <- 1 to 5) yield Member(i)

    val problem = Scenario.root(new Ensemble {
      name("utility root")

      class SubEnsemble(i: Int) extends Ensemble {
        name(s"sub$i")
        val member = oneOf(members)

        utility { member.sum(_.id) }
      }

      val subEnsembles = for (i <- 1 to 5) yield new SubEnsemble(i)
      rules(subEnsembles)
    })

    problem.resolve()
    for (subEns <- problem.instance.subEnsembles) {
      subEns.member.selectedMembers.head.id shouldBe 5
      subEns.solutionUtility shouldBe 5
    }
    problem.solutionUtility shouldBe (5 * 5)
  }
}
