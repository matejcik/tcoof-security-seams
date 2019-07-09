package external

import cz.cuni.mff.d3s.enact._

import scala.language.reflectiveCalls

class OneOfTest extends TestClass {
  "oneOf" should "select one member" in {
    val members = for (i <- 1 to 5) yield Member(i)
    val problem = Policy.root(new Ensemble {
      val selectedMember = oneOf(members)
    })

    assert(problem.resolve())
    assert(problem.instance.selectedMember.selectedMembers.size == 1)
  }

  it should "select preferred member" in {
    for (sel <- 1 to 5) {
      val members = for (i <- 1 to 5) yield Member(i)
      val problem = Policy.root(new Ensemble {
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
      val problem = Policy.root(new Ensemble {
        val selectedMember = oneOf(members)
      })

      problem.init()
      for (_ <- 0 until n) assert(problem.solve())
      assert(!problem.solve())
    }
  }

  it should "work with roles" in {
    val members = for (i <- 1 to 5) yield Member(i)
    val problem = Policy.root(new Ensemble {
      val subset = subsetOf(members)
      val selectedMember = oneOf(subset)
    })

    problem.init()
    while (problem.solve()) {
      assert(problem.instance.selectedMember.selectedMembers.size == 1)
      problem.instance.subset.selectedMembers should contain(
        problem.instance.selectedMember.selectedMembers.head
      )
    }
    assert(problem.exists)
  }

  "unsolvable problem" should "have no solution" in {
    val members = Seq.empty[Member]
    val problem = Policy.root(new Ensemble {
      val selectedMember = oneOf(members)
    })

    assert(!problem.resolve())
  }
}
