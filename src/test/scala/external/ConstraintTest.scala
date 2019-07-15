package external

import cz.cuni.mff.d3s.trust._

import scala.language.reflectiveCalls

class ConstraintTest extends TestClass {
  "allEqual" should "ensure all are equal" in {
    val members = for (i <- 1 to 25) yield Member(i)

    val problem = Policy.root(new Ensemble {
      val selection = subsetOf(members, _ === 5)

      constraint { selection.allEqual(_.id % 5) }
    })

    problem.init()
    var solutions = 0
    while (problem.solve()) {
      val ids = problem.instance.selection.selectedMembers.map(_.id)
      ids should have size 5
      assert(ids.forall(_ % 5 == ids.head % 5))
      solutions += 1
    }

    solutions shouldBe 5
  }

  it should "work with empty roles" in {
    val problem = Policy.root(new Ensemble {
      val emptyList = Seq.empty[Component]
      val role = subsetOf(emptyList)
      constraint { role.allEqual(x => x) }
    })

    assert(problem.resolve())
  }

  "allDifferent" should "ensure all are different" in {
    val members = for (i <- 1 to 25) yield Member(i)

    val problem = Policy.root(new Ensemble {
      val selection = subsetOf(members, _ === 5)

      constraint { selection.allDifferent(_.id % 5) }
    })

    problem.init()
    var solutions = 0
    while (problem.solve()) {
      val ids = problem.instance.selection.selectedMembers.map(_.id % 5)
      ids should have size 5
      for (i <- 0 to 4)
        ids should contain(i)
      solutions += 1
    }

    solutions shouldBe math.pow(5, 5)
  }

  it should "work with empty roles" in {
    val problem = Policy.root(new Ensemble {
      val emptyList = Seq.empty[Component]
      val role = subsetOf(emptyList)
      constraint { role.allDifferent(x => x) }
    })

    assert(problem.resolve())
  }

  "all" should "ensure application to all members" in {
    val members = for (i <- 1 to 25) yield Member(i)

    val problem = Policy.root(new Ensemble {
      val selection = subsetOf(members, _ > 1)

      constraint { selection.all(_.id % 5 == 1) }
    })

    problem.init()
    while (problem.solve()) {
      val ids = problem.instance.selection.selectedMembers.map(_.id)
      ids.size should be > 1
      assert(ids.forall(_ % 5 == 1))
    }
    assert(problem.exists)
  }

  it should "work with logicals" in {
    val members = for (i <- 1 to 25) yield Member(i)

    val problem = Policy.root(new Ensemble {
      val selection = subsetOf(members, _ > 1)

      constraint {
        val logical = selection.cardinality > 0
        selection.all(x => (x.id % 5 == 1) && logical)
      }
    })

    problem.init()
    while (problem.solve()) {
      val ids = problem.instance.selection.selectedMembers.map(_.id)
      ids.size should be > 1
      assert(ids.forall(_ % 5 == 1))
    }
    assert(problem.exists)
  }

  "some" should "ensure application to at least one member" in {
    val members = for (i <- 1 to 25) yield Member(i)

    val problem = Policy.root(new Ensemble {
      val selection = subsetOf(members, _ === 3)

      constraint {
        selection.some(_.id % 5 == 1) &&
        selection.some(_.id % 5 != 1)
      }
    })

    problem.init()
    while (problem.solve()) {
      val ids = problem.instance.selection.selectedMembers.map(_.id)
      ids.size should be > 1
      assert(ids.exists(_ % 5 == 1))
      assert(ids.exists(_ % 5 != 1))
    }
    assert(problem.exists)
  }

  it should "work with logicals" in {
    val members = for (i <- 1 to 25) yield Member(i)

    val problem = Policy.root(new Ensemble {
      val selection = subsetOf(members, _ === 3)

      constraint {
        val logical = selection.cardinality > 0
        selection.some(x => (x.id % 5 == 1) && logical) &&
        selection.some(x => (x.id % 5 != 1) && logical)
      }
    })

    problem.init()
    while (problem.solve()) {
      val ids = problem.instance.selection.selectedMembers.map(_.id)
      ids.size should be > 1
      assert(ids.exists(_ % 5 == 1))
      assert(ids.exists(_ % 5 != 1))
    }
    assert(problem.exists)
  }
}
