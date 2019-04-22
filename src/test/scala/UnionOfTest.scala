import scala.language.reflectiveCalls
import org.scalatest.{FlatSpec, Matchers}
import tcof.RootEnsemble

class UnionOfTest extends FlatSpec with ModelSolver with Matchers {
  "union" should "contain same members as its parents" in {
    val membersA = for (i <- 1 to 10) yield Member(i)
    val membersB = for (i <- 100 to 110) yield Member(i)
    val problem = root(new RootEnsemble {
      val a = oneOf(membersA)
      val b = oneOf(membersB)
      val union = unionOf(a, b)
    })

    assert(problem.resolve())

    val a = problem.instance.a.selectedMembers.head
    val b = problem.instance.b.selectedMembers.head
    val union = problem.instance.union.selectedMembers

    union should have size 2
    union should contain (a)
    union should contain (b)
  }
}
