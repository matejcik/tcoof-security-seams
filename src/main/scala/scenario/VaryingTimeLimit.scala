package scenario

import cz.cuni.mff.d3s.enact.Policy
import org.chocosolver.util.tools.TimeUtils
import scenario.testing._
import scenario.lunch._
import scenario.lunch.LunchSpec._

case class VaryingTimeLimit(limit: Long) extends Spec[LunchScenario] {
  val DefaultScenario = LunchSpec(
    projects = 3,
    lunchrooms = (5, 20),
    workrooms = (1, 1),
    workers = 5000,
    hungryWorkers = 21,
    fillRooms = 0,
    isLunchTime = true,
  )

  override def makeScenario(): LunchScenario = DefaultScenario.makeScenario()
  override def policy(scenario: LunchScenario): Policy[_] = scenario.policy
}

object VaryingTimeLimitTest extends TestHarness[LunchScenario] {
  override type ScenarioSpec = VaryingTimeLimit

  override def solveScenario(spec: ScenarioSpec): Measure = {
    val model = spec.makeScenario()
    val policy = spec.policy(model)

    val start = System.nanoTime()
    policy.init()
    policy.solverLimitTime(spec.limit)
    while (policy.solve()) {}
    val end = System.nanoTime()
    val time = end - start

    val success = policy.exists
    val utility = if (policy.exists) policy.solutionUtility else -1

    Measure(success, time, utility)
  }

  override val TEST_ROUNDS: Int = 10

  def main(args: Array[String]): Unit = {
    measure(
      "timelimits",
      "resulting utility with increasing time limit",
    ) { m =>
    warmup(VaryingTimeLimit(1000))
      for (timeLimit <- 1 to 60) {
        m(VaryingTimeLimit(timeLimit * 1000L))
      }
    }
  }
}
