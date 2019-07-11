package scenario

import cz.cuni.mff.d3s.enact._
import scenario.testing.{Spec, TestHarness}
import scala.util.control.Breaks._

class ManyVarScenario(ints: Int, bools: Int, constraints: Int) {
  case class Member(id: Int) extends Component

  val members = for (i <- 1 to 100000) yield Member(i)

  val policy = Policy.root(new Ensemble {
    val role = subsetOf(members)

    constraint {
      var card: Integer = role.cardinality
      for (_ <- 0 to ints) {
        card += 15
        card -= 15
      }
      var result = card === 1
      for (_ <- 0 to bools) result &&= card === 1
      result
    }

    for (x <- 2 to constraints) {
      constraint(role.cardinality < x)
    }
  })
}

case class ManyVarSpec(ints: Int, bools: Int, constraints: Int) extends Spec[ManyVarScenario] {
  override def makeScenario(): ManyVarScenario = new ManyVarScenario(ints, bools, constraints)
  override def policy(scenario: ManyVarScenario): Policy[_] = scenario.policy
}

object Variables extends TestHarness[ManyVarScenario] {

  override def solveScenario(spec: ScenarioSpec): Measure = {
    val model = spec.makeScenario()
    val policy = spec.policy(model)

    policy.init()
    policy.solverLimitTime(SOLVER_TIME_LIMIT)
    val start = System.nanoTime()
    policy.resolve()
    val end = System.nanoTime()
    val time = end - start

    val success = time < LIMIT_NANO
    Measure(success, time, policy.solutionUtility)
  }

  def measure_manyIntVars =
    measure(
      "integers",
      "creating a LOT of IntVars through repeated arithmetic",
    ) { m =>
      warmup(ManyVarSpec(250, 0, 0))

      for (intCount <- 500.to(10000, 500)) {
        val spec = ManyVarSpec(intCount, 0, 0)
        m(spec)
      }
    }

  def measure_manyBoolVars =
    measure(
      "booleans",
      "creating a LOT of SAT constraints through repeated boolean ops",
    ) { m =>
      warmup(ManyVarSpec(0, 250, 0))

      breakable {
        for (boolCount <- 500.to(10000, 500)) {
          val spec = ManyVarSpec(0, boolCount, 0)
          if (!m(spec)) break()
        }
      }
    }

  def measure_manyConstraints =
    measure(
      "constraints",
      "creating a LOT of separate membership constraints",
    ) { m =>
      warmup(ManyVarSpec(0, 0, 250))

      for (constraintCount <- 500.to(10000, 500)) {
        val spec = ManyVarSpec(0, 0, constraintCount * 100)
        m(spec)
      }
    }

  def main(args: Array[String]): Unit = {
    measure_manyIntVars
    measure_manyConstraints
    measure_manyBoolVars
  }
}
