package external

import cz.cuni.mff.d3s.enact._

import scala.language.reflectiveCalls

class SubsetOfTest extends TestClass {
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

  def fact(n: Int): Int = (1 to n).foldLeft(1)(_ * _)

  def choose(n: Int, k: Int): Int = fact(n) / (fact(k) * fact(n - k))

  it should "have N choose K solutions" in {
    val N = 10
    val K = 3

    val members = for (i <- 1 to N) yield Member(i)
    val problem = Policy.root(new Ensemble {
      val selection = subsetOf(members, _ === K)
    })

    problem.init()
    for (_ <- 0 until choose(N, K)) assert(problem.solve())
    assert(!problem.solve())
  }

  it should "allow subsets of subsets" in {
    val N  = 10
    val K1 = 5
    val K2 = 3

    val members = for (i <- 1 to N) yield Member(i)
    val problem = Policy.root(new Ensemble {
      val selection = subsetOf(members, _ === K1)
      val subselection = subsetOf(selection, _ === K2)
    })

    problem.init()
    val solutions = choose(N, K1) * choose(K1, K2)
    for (_ <- 1 to solutions) {
      assert(problem.solve())
      for (elem <- problem.instance.subselection.selectedMembers)
        problem.instance.selection.selectedMembers should contain(elem)
    }
    assert(!problem.solve())
  }
}
