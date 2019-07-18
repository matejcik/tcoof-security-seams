package external

import cz.cuni.mff.d3s.trust._

class AllowsTest extends TestClass {
  "allows" should "deny by default" in {
    var policy = Policy.root(new Ensemble {})

    assert(policy.resolve())
    assert(!policy.allows(Member(1), "greet", Member(2)))
  }

  it should "allow when granted" in {
    var policy = Policy.root(new Ensemble {
      val actor = oneOf(Member(1))
      val subject = oneOf(Member(2))

      allow(actor, "action", subject)
    })

    assert(policy.resolve())
    assert(policy.allows(Member(1), "action", Member(2)))
  }

  it should "deny by default when allow grants exist" in {
    var policy = Policy.root(new Ensemble {
      val actor = oneOf(Member(1))
      val subject = oneOf(Member(2))

      allow(actor, "action", subject)
    })

    assert(policy.resolve())
    assert(policy.allows(Member(1), "action", Member(2)))
    assert(!policy.allows(Member(1), "action", Member(3)))
    assert(!policy.allows(Member(1), "greet", Member(2)))
    assert(!policy.allows(Member(3), "greet", Member(2)))
  }

  it should "deny when both granted and denied" in {
    var policy = Policy.root(new Ensemble {
      val actor = oneOf(Member(1))
      val subject = oneOf(Member(2))

      allow(actor, "action", subject)
      deny(actor, "action", subject)
    })

    assert(policy.resolve())
    assert(!policy.allows(Member(1), "action", Member(2)))
  }

  it should "deny when only denied" in {
    var policy = Policy.root(new Ensemble {
      val actor = oneOf(Member(1))
      val subject = oneOf(Member(2))

      deny(actor, "action", subject)
    })

    assert(policy.resolve())
    assert(!policy.allows(Member(1), "action", Member(2)))
  }

  it should "allow when mismatching deny grants are present" in {
    var policy = Policy.root(new Ensemble {
      val actor = oneOf(Member(1))
      val subject = oneOf(Member(2))
      val other = oneOf(Member(3))

      allow(actor, "action", subject)
      deny(actor, "action", other)
    })

    assert(policy.resolve())
    assert(policy.allows(Member(1), "action", Member(2)))
  }
}
