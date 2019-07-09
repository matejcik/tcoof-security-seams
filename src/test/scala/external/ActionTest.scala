package external

import cz.cuni.mff.d3s.enact._

class ActionTest extends TestClass {
  case object TestMessage extends Notification

  "actions" should "collect" in {
    val members = for (i <- 1 to 5) yield Member(i)
    val problem = Policy.root(new Ensemble {
      allow(members.head, "xyz", members.last)
      deny(members.last, "abc", members.head)
      notify(members.head, TestMessage)
    })

    assert(problem.resolve())
    problem.actions should have size 3
    problem.actions should contain(AllowAction(Member(1), "xyz", Member(5)))
    problem.actions should contain(DenyAction(Member(5), "abc", Member(1)))
    problem.actions should contain(NotifyAction(Member(1), TestMessage))
  }

  it should "collect recursively" in {
    val component = new Component {}
    val problem = Policy.root(new Ensemble {
      object SubEnsemble extends Ensemble {
        object SubSubEnsemble extends Ensemble {
          allow(Member(1), "xyz", component)
        }

        object SecondSubEnsemble extends Ensemble {
          allow(Member(2), "xyz", component)
        }

        allow(Member(3), "xyz", component)
        rules(SubSubEnsemble, SecondSubEnsemble)
      }

      allow(Member(4), "xyz", component)
      rules(SubEnsemble)
    })

    assert(problem.resolve())
    problem.actions should have size 4
    problem.actions should contain(AllowAction(Member(1), "xyz", component))
    problem.actions should contain(AllowAction(Member(2), "xyz", component))
    problem.actions should contain(AllowAction(Member(3), "xyz", component))
    problem.actions should contain(AllowAction(Member(4), "xyz", component))
  }

  it should "accept roles and collections" in {
    val members = for (i <- 1 to 5) yield Member(i)
    val problem = Policy.root(new Ensemble {
      val role = subsetOf(members, _ === 2)
      allow(role, "xyz", members)
    })

    problem.init()
    while (problem.solve()) {
      for (actor <- problem.instance.role.selectedMembers; subject <- members) {
        problem.actions should contain(AllowAction(actor, "xyz", subject))
      }
    }
    assert(problem.exists)
  }
}
