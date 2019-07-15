package scenario.testing

import scenario.lunch._
import scenario.lunch.LunchSpec._

object MeasureStaticAssignment extends TestHarness[LunchScenario] {
  override type ScenarioSpec = LunchSpec

  def measure_workerCount_simple =
    measure("workercount-simple", "varying worker count - not lunch hour") { m =>
      val defaultSpec = LunchSpec(
        projects = 40,
        lunchrooms = (0, 0),
        workrooms = (100, 50),
        workers = 50,
        hungryWorkers = 0,
        fillRooms = 0,
        isLunchTime = false,
      )
      warmup(defaultSpec)
      for (projectCount <- Seq(5, 15, 50)) {
        for (workerCount <- 500.to(10000, 500)) {
          val spec = defaultSpec.copy(
            projects = projectCount,
            workers = workerCount,
          )
          m(spec)
        }
      }
    }

  override val TEST_ROUNDS: Int = 500

  def main(args: Array[String]): Unit = {
    measure_workerCount_simple
  }
}
