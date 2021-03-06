package external

import cz.cuni.mff.d3s.trust._

import scala.language.reflectiveCalls

class UnionOfTest extends TestClass {
  "union" should "contain same members as its parents" in {
    val membersA = for (i <- 1 to 10) yield Member(i)
    val membersB = for (i <- 101 to 110) yield Member(i)
    val problem = Policy.root(new Ensemble {
      val a = oneOf(membersA)
      val b = oneOf(membersB)
      val union = unionOf(a, b)
    })

    assert(problem.resolve())

    val a = problem.instance.a.selectedMembers.head
    val b = problem.instance.b.selectedMembers.head
    val union = problem.instance.union.selectedMembers

    union should have size 2
    union should contain(a)
    union should contain(b)
  }

  it should "only have one copy of each member" in {
    val members = for (i <- 1 to 10) yield Member(i)
    val problem = Policy.root(new Ensemble {
      val a = oneOf(members)
      val b = oneOf(members)
      val union = unionOf(a, b)

      constraint { a.all(b.contains(_)) }
    })

    assert(problem.resolve())

    val a = problem.instance.a.selectedMembers.head
    val b = problem.instance.b.selectedMembers.head
    val union = problem.instance.union.selectedMembers

    union should have size 1
    union should contain(a)
    union should contain(b)
  }

  it should "affect selection in parents" in {
    val members = for (i <- 1 to 5) yield Member(i)
    val problem = Policy.root(new Ensemble {
      val a = oneOf(members)
      val b = oneOf(members)
      val union = unionOf(a, b)

      constraint { union.cardinality === 2 }
    })

    problem.init()
    while (problem.solve()) {
      val a = problem.instance.a.selectedMembers.head
      val b = problem.instance.b.selectedMembers.head
      val union = problem.instance.union.selectedMembers

      union should have size 2
      a should not equal (b)
    }
  }

  it should "work with empty roles" in {
    val problem = Policy.root(new Ensemble {
      val emptyList = Seq.empty[Component]
      val emptyRole = subsetOf(emptyList)
      val union = unionOf(emptyList, emptyRole)
    })

    assert(problem.resolve())
  }

  it should "allow empty roles from non-empty sets" in {
    val problem = Policy.root(new Ensemble {
      val emptyList = Seq.empty[Component]
      val nonemptyList = Seq(Member(1))
      val subset = subsetOf(nonemptyList)
      val union = unionOf(emptyList, subset)

      constraint { subset.cardinality === 0 }
    })

    assert(problem.resolve())
  }

  it should "allow empty roles in sub-ensembles" in {
    val problem = Policy.root(new Ensemble {
      class Ens extends Ensemble {
        val emptyList = Seq.empty[Component]
        val nonemptyList = Seq(Member(1))
        val subset = subsetOf(nonemptyList, _ <= 5)
        val union = unionOf(emptyList, subset)

        constraint { subset.cardinality === 0 }
      }
      val ens = new Ens
      rules(ens)
    })

    assert(problem.resolve())
  }
}
