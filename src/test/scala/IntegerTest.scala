import scala.language.reflectiveCalls
import tcof._

class IntegerTest extends ModelSolver {
  "integer" should "compare with ==" in {
    val problem = Scenario.root(new Ensemble {
      val roleA = oneOf(Member(1))
      val roleB = oneOf(Member(2))
    })

    problem.resolve()
    val roleA = problem.instance.roleA
    val roleB = problem.instance.roleB
    roleA.cardinality shouldBe an [Integer]
    (roleA.cardinality == roleB.cardinality) shouldBe a [Logical]
    (roleA.cardinality == 1) shouldBe a [Logical]
    true should not be a [Logical]
  }

}
