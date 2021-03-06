package scenario.testing

import cz.cuni.mff.d3s.trust.Policy
import scenario.lunch.LunchSpec._
import scenario.lunch._

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

/** Measure best utility with increasing time limit.
  *
  * @see section 7.3.4
  */
object MeasureVaryingTimeLimit extends TestHarness[LunchScenario] {
  override type ScenarioSpec = VaryingTimeLimit

  override def solveScenario(spec: ScenarioSpec): Measure = {
    val model = spec.makeScenario()
    val policy = spec.policy(model)

    val start = System.nanoTime()
    policy.resolve(spec.limit)
    val end = System.nanoTime()
    val time = end - start

    val success = policy.exists
    val utility = if (policy.exists) policy.solutionUtility else -1

    Measure(success, time, utility)
  }

  def main(args: Array[String]): Unit = {
    measure(
      "timelimits",
      "resulting utility with increasing time limit",
    ) { m =>
    warmup(VaryingTimeLimit(1000))
      for (timeLimit <- 1 to 30) {
        m(VaryingTimeLimit(timeLimit * 1000L))
      }
    }
  }
}
