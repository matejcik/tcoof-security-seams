import scala.language.reflectiveCalls
import tcof._

class ConstraintTest extends ModelSolver {
  "allEqual" should "ensure all are equal" in {
    val members = for (_ <- 1 to 5; i <- 1 to 5) yield Member(i)

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

  "contains" should "only accept appropriate component types" in {
    class Foo(id: Int) extends Component
    class Bar(id: Int) extends Component
    object Qux extends Foo(99)

    val foos = for (i <- 1 to 5) yield new Foo(i)
    val bars = for (i <- 1 to 5) yield new Bar(i)

    val problem = Scenario.root(new Ensemble {
      val foo = oneOf(foos)
      constraint { foo.contains(Qux) }
    })
  }
}
