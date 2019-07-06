import scala.language.reflectiveCalls
import tcof._

class SubsetOfTest extends ModelSolver {
  "problem" should "contain given number of members" in {
    for (n <- 3 to 10) {
      val members = for (i <- 1 to 10) yield Member(i)
      val problem = Policy.root(new Ensemble {
        val selection = subsetOf(members, _ === n)
      })

      assert(problem.resolve())
      assert(problem.instance.selection.selectedMembers.size == n)
    }
  }

  it should "have N choose K solutions" in {
    val N = 10
    val K = 3
    def fact(n: Int): Int = (1 to n).foldLeft(1)(_ * _)
    val NchooseK: Int = fact(N) / (fact(K) * fact(N - K))

    val members = for (i <- 1 to N) yield Member(i)
    val problem = Policy.root(new Ensemble {
      val selection = subsetOf(members, _ === K)
    })

    problem.init()
    for (_ <- 0 until NchooseK) assert(problem.solve())
    assert(!problem.solve())
  }
}
