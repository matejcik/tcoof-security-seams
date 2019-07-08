package external

import cz.cuni.mff.d3s.enact._

import scala.language.reflectiveCalls

class SubensembleTest extends TestClass {
  "multiple subensembles" should "behave identically with multiple rules calls" in {
    val members = for (i <- 1 to 10) yield Member(i)

  }

  "subensemble" should "not select members?" in {
    val members = for (i <- 1 to 10) yield Member(i)
    val door = new Component { name("door") }

    val problem = Policy.root(new Ensemble {

      class Ens extends Ensemble {

        situation { true }

        val x = subsetOf(members, _ > 1)

        constraint { x.cardinality === 2 }

        allow(x, "open", door)

        //utility { x.sum { v => if (v.id % 3 == 0) v.id else -v.id } }
      }

      val enses = for (_ <- 1 to 5) yield new Ens

      utility {
        enses.map(_.x.cardinality).reduce(_ + _) / enses.size
      }

      rules(enses)

    })

    problem.resolve()
    //println(problem.instance.enses.x.selectedMembers)
    // TODO
  }
}
