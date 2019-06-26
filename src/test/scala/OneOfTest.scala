import scala.language.reflectiveCalls
import tcof._

class OneOfTest extends ModelSolver {
  "problem" should "contain one member" in {
    val members = for (i <- 1 to 5) yield Member(i)
    val problem = root(new RootEnsemble {
      val selectedMember = oneOf(members)
    })

    assert(problem.resolve())
    assert(problem.instance.selectedMember.selectedMembers.size == 1)
  }

  it should "contain selected member" in {
    for (sel <- 1 to 5) {
      val members = for (i <- 1 to 5) yield Member(i)
      val problem = root(new RootEnsemble {
        val selectedMember = oneOf(members)

        constraint(selectedMember.contains(Member(sel)))
      })

      assert(problem.resolve())
      val selectedMember = problem.instance.selectedMember.selectedMembers.head
      assert(selectedMember == Member(sel))
    }
  }

  it should "have as many solutions as there are members" in {
    for (n <- 1 to 20) {
      val members = for (i <- 1 to n) yield Member(i)
      val problem = root(new RootEnsemble {
        val selectedMember = oneOf(members)
      })

      problem.init()
      for (_ <- 0 until n) assert(problem.solve())
      assert(!problem.solve())
    }
  }

  "unsolvable problem" should "have no solution" in {
    val members = Seq.empty[Member]
    val problem = root(new RootEnsemble {
      val selectedMember = oneOf(members)
    })

    assert(!problem.resolve())
  }
}
