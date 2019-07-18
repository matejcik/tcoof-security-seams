package external

import cz.cuni.mff.d3s.trust._

class MemberCollectionTest extends TestClass {
  "collection of roles" should "have cardinality" in {
    val members = for (i <- 1 to 5) yield Member(i)

    val policy = Policy.root(new Ensemble {
      val subsetA = subsetOf(members)
      val subsetB = subsetOf(members)

      val subsets = Seq(subsetA, subsetB)
      constraint { subsets.cardinality === 4 }
    })

    policy.init()
    while (policy.solve()) {
      val subsets = policy.instance.subsets
      val members = subsets.flatMap(_.selectedMembers)
      members should have size 4
    }
    assert(policy.exists)
  }

  it should "have disjointness" in {
    val members = for (i <- 1 to 5) yield Member(i)

    val policy = Policy.root(new Ensemble {
      val subsetA = subsetOf(members, _ === 2)
      val subsetB = subsetOf(members, _ === 3)

      val subsets = Seq(subsetA, subsetB)
      constraint { subsets.allDisjoint }
    })

    policy.init()
    while (policy.solve()) {
      val subsets = policy.instance.subsets
      val members = subsets.flatMap(_.selectedMembers).toSet
      members should have size 5
    }
    assert(policy.exists)
  }
}
