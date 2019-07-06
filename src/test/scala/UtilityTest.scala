import scala.language.reflectiveCalls
import tcof._

class UtilityTest extends ModelSolver {
  "utility" should "change result of solving" in {
    val members = for (i <- 1 to 5) yield Member(i)

    val noUtilitySolution = Scenario.root(new Ensemble {
      val member = subsetOf(members)
    })
    val utilitySolution = Scenario.root(new Ensemble {
      val member = subsetOf(members)

      utility { -member.cardinality }
    })

    noUtilitySolution.resolve()
    utilitySolution.resolve()

    // this relies on an assumption that the solver starts with all
    // members selected and refines down
    noUtilitySolution.instance.member.selectedMembers should have size 5
    utilitySolution.instance.member.selectedMembers should have size 0
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
